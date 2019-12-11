package com.acrylplatform.settings

import com.typesafe.config.Config
import com.acrylplatform.metrics.Metrics
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.concurrent.duration.FiniteDuration

case class AcrylSettings(directory: String,
                         ntpServer: String,
                         dbSettings: DBSettings,
                         extensions: Seq[String],
                         extensionsShutdownTimeout: FiniteDuration,
                         networkSettings: NetworkSettings,
                         walletSettings: WalletSettings,
                         blockchainSettings: BlockchainSettings,
                         minerSettings: MinerSettings,
                         restAPISettings: RestAPISettings,
                         synchronizationSettings: SynchronizationSettings,
                         utxSettings: UtxSettings,
                         featuresSettings: FeaturesSettings,
                         metrics: Metrics.Settings,
                         nodeStatus: Boolean,
                         config: Config)

object AcrylSettings extends CustomValueReaders {
  def fromRootConfig(rootConfig: Config): AcrylSettings = {
    val acryl = rootConfig.getConfig("acryl")

    val directory                 = acryl.as[String]("directory")
    val ntpServer                 = acryl.as[String]("ntp-server")
    val dbSettings                = acryl.as[DBSettings]("db")
    val extensions                = acryl.as[Seq[String]]("extensions")
    val extensionsShutdownTimeout = acryl.as[FiniteDuration]("extensions-shutdown-timeout")
    val networkSettings           = acryl.as[NetworkSettings]("network")
    val walletSettings            = acryl.as[WalletSettings]("wallet")
    val blockchainSettings        = acryl.as[BlockchainSettings]("blockchain")
    val minerSettings             = acryl.as[MinerSettings]("miner")
    val restAPISettings           = acryl.as[RestAPISettings]("rest-api")
    val synchronizationSettings   = acryl.as[SynchronizationSettings]("synchronization")
    val utxSettings               = acryl.as[UtxSettings]("utx")
    val featuresSettings          = acryl.as[FeaturesSettings]("features")
    val metrics                   = acryl.as[Metrics.Settings]("metrics")
    val nodeStatus                = acryl.as[Boolean]("node-status")

    AcrylSettings(
      directory,
      ntpServer,
      dbSettings,
      extensions,
      extensionsShutdownTimeout,
      networkSettings,
      walletSettings,
      blockchainSettings,
      minerSettings,
      restAPISettings,
      synchronizationSettings,
      utxSettings,
      featuresSettings,
      metrics,
      nodeStatus,
      rootConfig
    )
  }
}
