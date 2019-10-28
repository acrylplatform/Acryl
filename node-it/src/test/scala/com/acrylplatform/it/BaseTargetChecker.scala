package com.acrylplatform.it

import com.typesafe.config.ConfigFactory.{defaultApplication, defaultReference}
import com.acrylplatform.account.PublicKey
import com.acrylplatform.block.Block
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.consensus.PoSSelector
import com.acrylplatform.db.openDB
import com.acrylplatform.history.StorageFactory
import com.acrylplatform.settings._
import com.acrylplatform.transaction.Asset.Acryl
import com.acrylplatform.utils.NTP
import monix.execution.UncaughtExceptionReporter
import monix.reactive.Observer
import net.ceedubs.ficus.Ficus._

object BaseTargetChecker {
  def main(args: Array[String]): Unit = {
    val sharedConfig = Docker.genesisOverride
      .withFallback(Docker.configTemplate)
      .withFallback(defaultApplication())
      .withFallback(defaultReference())
      .resolve()

    val settings          = AcrylSettings.fromRootConfig(sharedConfig)
    val db                = openDB("/tmp/tmp-db")
    val ntpTime           = new NTP("ntp.pool.org")
    val portfolioChanges  = Observer.empty(UncaughtExceptionReporter.default)
    val blockchainUpdater = StorageFactory(settings, db, ntpTime, portfolioChanges)
    val poSSelector       = new PoSSelector(blockchainUpdater, settings.blockchainSettings, settings.synchronizationSettings)

    try {
      val genesisBlock = Block.genesis(settings.blockchainSettings.genesisSettings).explicitGet()
      blockchainUpdater.processBlock(genesisBlock)

      NodeConfigs.Default.map(_.withFallback(sharedConfig)).collect {
        case cfg if cfg.as[Boolean]("acryl.miner.enable") =>
          val account   = PublicKey(cfg.as[ByteStr]("public-key").arr)
          val address   = account.toAddress
          val balance   = blockchainUpdater.balance(address, Acryl)
          val consensus = genesisBlock.consensusData
          val timeDelay = poSSelector
            .getValidBlockDelay(blockchainUpdater.height, account, consensus.baseTarget, balance)
            .explicitGet()

          f"$address: ${timeDelay * 1e-3}%10.3f s"
      }
    } finally ntpTime.close()
  }
}
