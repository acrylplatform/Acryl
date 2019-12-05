package com.acrylplatform

import java.io.File
import java.net.InetSocketAddress
import java.security.Security
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.stream.ActorMaterializer
import cats.instances.all._
import com.typesafe.config._
import com.acrylplatform.account.{Address, AddressScheme}
import com.acrylplatform.actor.RootActorSystem
import com.acrylplatform.api.http._
import com.acrylplatform.api.http.alias.{AliasApiRoute, AliasBroadcastApiRoute}
import com.acrylplatform.api.http.assets.{AssetsApiRoute, AssetsBroadcastApiRoute}
import com.acrylplatform.api.http.leasing.{LeaseApiRoute, LeaseBroadcastApiRoute}
import com.acrylplatform.consensus.PoSSelector
import com.acrylplatform.consensus.nxt.api.http.NxtConsensusApiRoute
import com.acrylplatform.db.openDB
import com.acrylplatform.extensions.{Context, Extension}
import com.acrylplatform.features.api.ActivationApiRoute
import com.acrylplatform.history.StorageFactory
import com.acrylplatform.http.{AcrylApiRoute, DebugApiRoute, NodeApiRoute}
import com.acrylplatform.metrics.Metrics
import com.acrylplatform.mining.{Miner, MinerImpl}
import com.acrylplatform.network.RxExtensionLoader.RxExtensionLoaderShutdownHook
import com.acrylplatform.network._
import com.acrylplatform.settings.AcrylSettings
import com.acrylplatform.state.Blockchain
import com.acrylplatform.state.appender.{BlockAppender, ExtensionAppender, MicroblockAppender}
import com.acrylplatform.transaction.{Asset, Transaction}
import com.acrylplatform.utils.{LoggerFacade, NTP, NodeStatus, ScorexLogging, SystemInformationReporter, Time, UtilApp}
import com.acrylplatform.utx.{UtxPool, UtxPoolImpl}
import com.acrylplatform.wallet.Wallet
import io.netty.channel.Channel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import kamon.Kamon
import kamon.influxdb.InfluxDBReporter
import kamon.system.SystemMetrics
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import monix.execution.Scheduler._
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import org.influxdb.dto.Point
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Try
import scala.util.control.NonFatal

class Application(val actorSystem: ActorSystem, val settings: AcrylSettings, configRoot: ConfigObject, time: NTP) extends ScorexLogging {
  app =>

  import monix.execution.Scheduler.Implicits.{global => scheduler}
  private[this] val apiScheduler = Scheduler(actorSystem.dispatcher)

  private val db = openDB(settings.dbSettings.directory)

  private val LocalScoreBroadcastDebounce = 1.second

  private val spendableBalanceChanged = ConcurrentSubject.publish[(Address, Asset)]

  private val blockchainUpdater = StorageFactory(settings, db, time, spendableBalanceChanged)

  private lazy val upnp = new UPnP(settings.networkSettings.uPnPSettings) // don't initialize unless enabled

  private val wallet: Wallet = try {
    Wallet(settings.walletSettings)
  } catch {
    case NonFatal(e) =>
      log.error(s"Failed to open wallet file '${settings.walletSettings.file.get.getAbsolutePath}", e)
      throw e
  }

  private val peerDatabase = new PeerDatabaseImpl(settings.networkSettings)

  private val extensionLoaderScheduler        = singleThread("rx-extension-loader", reporter = log.error("Error in Extension Loader", _))
  private val microblockSynchronizerScheduler = singleThread("microblock-synchronizer", reporter = log.error("Error in Microblock Synchronizer", _))
  private val scoreObserverScheduler          = singleThread("rx-score-observer", reporter = log.error("Error in Score Observer", _))
  private val appenderScheduler               = singleThread("appender", reporter = log.error("Error in Appender", _))
  private val historyRepliesScheduler         = fixedPool("history-replier", poolSize = 2, reporter = log.error("Error in History Replier", _))
  private val minerScheduler                  = fixedPool("miner-pool", poolSize = 2, reporter = log.error("Error in Miner", _))

  private var rxExtensionLoaderShutdown: Option[RxExtensionLoaderShutdownHook] = None
  private var maybeUtx: Option[UtxPool]                                        = None
  private var maybeNetwork: Option[NS]                                         = None

  private var extensions = Seq.empty[Extension]

  def apiShutdown(): Unit = {
    for {
      u <- maybeUtx
      n <- maybeNetwork
    } yield shutdown(u, n)
  }

  def run(): Unit = {
    checkGenesis(settings, blockchainUpdater)

    if (wallet.privateKeyAccounts.isEmpty)
      wallet.generateNewAccounts(1)

    val establishedConnections = new ConcurrentHashMap[Channel, PeerInfo]
    val allChannels            = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    val utxStorage             = new UtxPoolImpl(time, blockchainUpdater, spendableBalanceChanged, settings.utxSettings)
    maybeUtx = Some(utxStorage)

    val knownInvalidBlocks = new InvalidBlockStorageImpl(settings.synchronizationSettings.invalidBlocksStorage)

    val pos = new PoSSelector(blockchainUpdater, settings.blockchainSettings, settings.synchronizationSettings)

    val miner =
      if (settings.minerSettings.enable)
        new MinerImpl(allChannels, blockchainUpdater, settings, time, utxStorage, wallet, pos, minerScheduler, appenderScheduler)
      else Miner.Disabled

    val processBlock =
      BlockAppender(blockchainUpdater, time, utxStorage, pos, allChannels, peerDatabase, miner, appenderScheduler) _

    val processFork =
      ExtensionAppender(blockchainUpdater, utxStorage, pos, time, knownInvalidBlocks, peerDatabase, miner, appenderScheduler) _
    val processMicroBlock =
      MicroblockAppender(blockchainUpdater, utxStorage, allChannels, peerDatabase, appenderScheduler) _

    import blockchainUpdater.lastBlockInfo

    val lastScore = lastBlockInfo
      .map(_.score)
      .distinctUntilChanged
      .share(scheduler)

    lastScore
      .debounce(LocalScoreBroadcastDebounce)
      .foreach { x =>
        allChannels.broadcast(LocalScoreChanged(x))
      }(scheduler)

    val networkSettingsCopy =
      if (settings.networkSettings.uPnPSettings.enable)
        settings.networkSettings.copy(
          declaredAddress = settings.networkSettings.declaredAddress match {
            case Some(address) =>
              for (addr <- settings.networkSettings.declaredAddress)
                upnp.addPort(addr.getPort) match {
                  case Left(address) => log.debug("Mapped port [" + address + "]:" + addr.getPort)
                  case Right(string) => log.error(string)
                }
              Option(address)
            case None =>
              upnp.addPort(settings.networkSettings.bindAddress.getPort) match {
                case Left(address) => Option(new InetSocketAddress(address.getHostAddress, settings.networkSettings.bindAddress.getPort))
                case Right(string) =>
                  log.error(string)
                  None
              }
          }
        )
      else
        settings.networkSettings.copy()

    val settingsForNetworkServer = settings.copy(
      networkSettings = networkSettingsCopy
    )

    val historyReplier = new HistoryReplier(blockchainUpdater, settings.synchronizationSettings, historyRepliesScheduler)
    val network =
      NetworkServer(settingsForNetworkServer,
                    lastBlockInfo,
                    blockchainUpdater,
                    historyReplier,
                    utxStorage,
                    peerDatabase,
                    allChannels,
                    establishedConnections)
    maybeNetwork = Some(network)
    val (signatures, blocks, blockchainScores, microblockInvs, microblockResponses, transactions) = network.messages

    val timeoutSubject: ConcurrentSubject[Channel, Channel] = ConcurrentSubject.publish[Channel]

    val (syncWithChannelClosed, scoreStatsReporter) = RxScoreObserver(
      settings.synchronizationSettings.scoreTTL,
      1.second,
      blockchainUpdater.score,
      lastScore,
      blockchainScores,
      network.closedChannels,
      timeoutSubject,
      scoreObserverScheduler
    )
    val (microblockData, mbSyncCacheSizes) = MicroBlockSynchronizer(
      settings.synchronizationSettings.microBlockSynchronizer,
      peerDatabase,
      lastBlockInfo.map(_.id),
      microblockInvs,
      microblockResponses,
      microblockSynchronizerScheduler
    )
    val (newBlocks, extLoaderState, sh) = RxExtensionLoader(
      settings.synchronizationSettings.synchronizationTimeout,
      Coeval(blockchainUpdater.lastBlockIds(settings.synchronizationSettings.maxRollback)),
      peerDatabase,
      knownInvalidBlocks,
      blocks,
      signatures,
      syncWithChannelClosed,
      extensionLoaderScheduler,
      timeoutSubject
    ) { case (c, b) => processFork(c, b.blocks) }

    rxExtensionLoaderShutdown = Some(sh)

    UtxPoolSynchronizer.start(utxStorage,
                              settings.synchronizationSettings.utxSynchronizer,
                              allChannels,
                              transactions,
                              blockchainUpdater.lastBlockInfo)

    val microBlockSink = microblockData
      .mapEval(scala.Function.tupled(processMicroBlock))

    val blockSink = newBlocks
      .mapEval(scala.Function.tupled(processBlock))

    Observable(microBlockSink, blockSink).merge.subscribe()

    miner.scheduleMining()

    implicit val as: ActorSystem                 = actorSystem
    implicit val materializer: ActorMaterializer = ActorMaterializer()

    val extensionContext = new Context {
      override def settings: AcrylSettings                               = app.settings
      override def blockchain: Blockchain                                = app.blockchainUpdater
      override def time: Time                                            = app.time
      override def wallet: Wallet                                        = app.wallet
      override def utx: UtxPool                                          = utxStorage
      override def broadcastTx(tx: Transaction): Unit                    = allChannels.broadcastTx(tx, None)
      override def spendableBalanceChanged: Observable[(Address, Asset)] = app.spendableBalanceChanged
      override def actorSystem: ActorSystem                              = app.actorSystem
    }

    extensions = settings.extensions.map { extensionClassName =>
      val extensionClass = Class.forName(extensionClassName).asInstanceOf[Class[Extension]]
      val ctor           = extensionClass.getConstructor(classOf[Context])
      log.info(s"Enable extension: $extensionClassName")
      ctor.newInstance(extensionContext)
    }

    if (settings.restAPISettings.enable) {
      val apiRoutes = Seq(
        NodeApiRoute(settings.restAPISettings, blockchainUpdater, () => apiShutdown()),
        BlocksApiRoute(settings.restAPISettings, blockchainUpdater, allChannels)(apiScheduler),
        TransactionsApiRoute(settings.restAPISettings, wallet, blockchainUpdater, utxStorage, allChannels, time)(apiScheduler),
        NxtConsensusApiRoute(settings.restAPISettings, blockchainUpdater),
        WalletApiRoute(settings.restAPISettings, wallet),
        PaymentApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, time),
        UtilsApiRoute(time, settings.restAPISettings),
        PeersApiRoute(settings.restAPISettings, network.connect, peerDatabase, establishedConnections),
        AddressApiRoute(settings.restAPISettings, wallet, blockchainUpdater, utxStorage, allChannels, time)(apiScheduler),
        DebugApiRoute(
          settings,
          time,
          blockchainUpdater,
          wallet,
          blockchainUpdater,
          peerDatabase,
          establishedConnections,
          blockId => Task(blockchainUpdater.removeAfter(blockId)).executeOn(appenderScheduler),
          allChannels,
          utxStorage,
          miner,
          historyReplier,
          extLoaderState,
          mbSyncCacheSizes,
          scoreStatsReporter,
          configRoot
        ),
        AcrylApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, time),
        AssetsApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, blockchainUpdater, time)(apiScheduler),
        ActivationApiRoute(settings.restAPISettings, settings.featuresSettings, blockchainUpdater),
        AssetsBroadcastApiRoute(settings.restAPISettings, utxStorage, allChannels),
        LeaseApiRoute(settings.restAPISettings, wallet, blockchainUpdater, utxStorage, allChannels, time),
        LeaseBroadcastApiRoute(settings.restAPISettings, utxStorage, allChannels),
        AliasApiRoute(settings.restAPISettings, wallet, utxStorage, allChannels, time, blockchainUpdater),
        AliasBroadcastApiRoute(settings.restAPISettings, utxStorage, allChannels)
      )

      val apiTypes: Set[Class[_]] = Set(
        classOf[NodeApiRoute],
        classOf[BlocksApiRoute],
        classOf[TransactionsApiRoute],
        classOf[NxtConsensusApiRoute],
        classOf[WalletApiRoute],
        classOf[UtilsApiRoute],
        classOf[PeersApiRoute],
        classOf[AddressApiRoute],
        classOf[DebugApiRoute],
        classOf[AssetsApiRoute],
        classOf[ActivationApiRoute],
        classOf[LeaseApiRoute],
        classOf[AliasApiRoute]
      )

      val combinedRoute = CompositeHttpService(apiTypes, apiRoutes, settings.restAPISettings)(actorSystem).loggingCompositeRoute
      val httpFuture    = Http().bindAndHandle(combinedRoute, settings.restAPISettings.bindAddress, settings.restAPISettings.port)
      serverBinding = Await.result(httpFuture, 20.seconds)
      log.info(s"REST API was bound on ${settings.restAPISettings.bindAddress}:${settings.restAPISettings.port}")
    }

    extensions.foreach(_.start())

    NodeStatus.start(settings.nodeStatus, blockchainUpdater, wallet, network, upnp.localAddress)

    // on unexpected shutdown
    sys.addShutdownHook {
      Await.ready(Kamon.stopAllReporters(), 20.seconds)
      Metrics.shutdown()
      shutdown(utxStorage, network)
    }
  }

  private val shutdownInProgress             = new AtomicBoolean(false)
  @volatile var serverBinding: ServerBinding = _

  def shutdown(utx: UtxPool, network: NS): Unit =
    if (shutdownInProgress.compareAndSet(false, true)) {

      if (extensions.nonEmpty) {
        log.info(s"Shutting down extensions")
        Await.ready(Future.sequence(extensions.map(_.shutdown())), settings.extensionsShutdownTimeout)
      }

      spendableBalanceChanged.onComplete()
      utx.close()

      shutdownAndWait(historyRepliesScheduler, "HistoryReplier", 5.minutes)

      log.info("Closing REST API")
      if (settings.restAPISettings.enable)
        Try(Await.ready(serverBinding.unbind(), 2.minutes)).failed.map(e => log.error("Failed to unbind REST API port", e))

      settings.networkSettings.declaredAddress match {
        case Some(_) =>
          for (addr <- settings.networkSettings.declaredAddress if settings.networkSettings.uPnPSettings.enable) {
            upnp.deletePort(addr.getPort)
          }
        case None =>
          if (settings.networkSettings.uPnPSettings.enable)
            upnp.deletePort(settings.networkSettings.bindAddress.getPort)
      }

      log.debug("Closing peer database")
      peerDatabase.close()

      Try(Await.result(actorSystem.terminate(), 2.minute)).failed.map(e => log.error("Failed to terminate actor system", e))
      log.debug("Node's actor system shutdown successful")

      blockchainUpdater.shutdown()
      rxExtensionLoaderShutdown.foreach(_.shutdown())

      log.info("Stopping network services")
      network.shutdown()

      shutdownAndWait(minerScheduler, "Miner")
      shutdownAndWait(microblockSynchronizerScheduler, "MicroblockSynchronizer")
      shutdownAndWait(scoreObserverScheduler, "ScoreObserver")
      shutdownAndWait(extensionLoaderScheduler, "ExtensionLoader")
      shutdownAndWait(appenderScheduler, "Appender", 5.minutes)

      log.info("Closing storage")
      db.close()

      time.close()
      log.info("Shutdown complete")
    }

  private def shutdownAndWait(scheduler: SchedulerService, name: String, timeout: FiniteDuration = 1.minute): Unit = {
    log.debug(s"Shutting down $name")
    scheduler.shutdown()
    val r = Await.result(scheduler.awaitTermination(timeout, global), 2 * timeout)
    if (r)
      log.info(s"$name was shutdown successfully")
    else
      log.warn(s"Failed to shutdown $name properly during timeout")
  }

}

object Application {
  private[acrylplatform] def loadApplicationConfig(external: Option[File] = None): AcrylSettings = {
    import com.acrylplatform.settings._

    val config = loadConfig(external.map(ConfigFactory.parseFile))

    // DO NOT LOG BEFORE THIS LINE, THIS PROPERTY IS USED IN logback.xml
    System.setProperty("acryl.directory", config.getString("acryl.directory"))

    val settings = AcrylSettings.fromRootConfig(config)

    // Initialize global var with actual address scheme
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    // IMPORTANT: to make use of default settings for histograms and timers, it's crucial to reconfigure Kamon with
    //            our merged config BEFORE initializing any metrics, including in settings-related companion objects
    Kamon.reconfigure(config)

    if (config.getBoolean("kamon.enable")) {
      Kamon.addReporter(new InfluxDBReporter())
      SystemMetrics.startCollecting()
    }

    settings
  }

  def main(args: Array[String]): Unit = {

    // prevents java from caching successful name resolutions, which is needed e.g. for proper NTP server rotation
    // http://stackoverflow.com/a/17219327
    System.setProperty("sun.net.inetaddr.ttl", "0")
    System.setProperty("sun.net.inetaddr.negative.ttl", "0")
    Security.setProperty("networkaddress.cache.ttl", "0")
    Security.setProperty("networkaddress.cache.negative.ttl", "0")

    // specify aspectj to use it's build-in infrastructure
    // http://www.eclipse.org/aspectj/doc/released/pdguide/trace.html
    System.setProperty("org.aspectj.tracing.factory", "default")

    args.headOption.getOrElse("") match {
      case "export"                 => Exporter.main(args.tail)
      case "import"                 => Importer.main(args.tail)
      case "explore"                => Explorer.main(args.tail)
      case "util"                   => UtilApp.main(args.tail)
      case "help" | "--help" | "-h" => println("Usage: acryl <config> | export | import | explore | util")
      case _                        => startNode(args.headOption)
    }
  }

  private[this] def startNode(configFile: Option[String]): Unit = {
    import com.acrylplatform.settings.Constants
    val settings = loadApplicationConfig(configFile.map(new File(_)))

    val log = LoggerFacade(LoggerFactory.getLogger(getClass))
    log.info("Starting...")
    sys.addShutdownHook {
      SystemInformationReporter.report(settings.config)
    }

    val time             = new NTP(settings.ntpServer)
    val isMetricsStarted = Metrics.start(settings.metrics, time)

    RootActorSystem.start("acrylplatform", settings.config) { actorSystem =>
      import actorSystem.dispatcher
      isMetricsStarted.foreach { started =>
        if (started) {
          import settings.synchronizationSettings.microBlockSynchronizer
          import settings.{minerSettings => miner}

          Metrics.write(
            Point
              .measurement("config")
              .addField("miner-micro-block-interval", miner.microBlockInterval.toMillis)
              .addField("miner-max-transactions-in-key-block", miner.maxTransactionsInKeyBlock)
              .addField("miner-max-transactions-in-micro-block", miner.maxTransactionsInMicroBlock)
              .addField("miner-min-micro-block-age", miner.minMicroBlockAge.toMillis)
              .addField("mbs-wait-response-timeout", microBlockSynchronizer.waitResponseTimeout.toMillis)
          )
        }
      }

      log.info(s"${Constants.AgentName} Blockchain Id: ${settings.blockchainSettings.addressSchemeCharacter}")

      new Application(actorSystem, settings, settings.config.root(), time).run()
    }
  }
}
