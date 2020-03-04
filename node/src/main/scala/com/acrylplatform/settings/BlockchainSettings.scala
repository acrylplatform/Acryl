package com.acrylplatform.settings

import com.typesafe.config.Config
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.features.BlockchainFeatures
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.EnumerationReader._
import net.ceedubs.ficus.readers.ValueReader

import scala.concurrent.duration._

case class RewardsSettings(initial: Long) {
  require(initial >= 0, "initial must be greater than or equal to 0")
}

object RewardsSettings {
  val MAINNET: RewardsSettings = apply(
    6 * Constants.UnitsInWave
  )

  val TESTNET: RewardsSettings = apply(
    6 * Constants.UnitsInWave
  )
}

case class FunctionalitySettings(featureCheckBlocksPeriod: Int,
                                 blocksForFeatureActivation: Int,
                                 allowTemporaryNegativeUntil: Long,
                                 generationBalanceDepthFrom50To1000AfterHeight: Int,
                                 minimalGeneratingBalanceAfter: Long,
                                 allowTransactionsFromFutureUntil: Long,
                                 allowUnissuedAssetsUntil: Long,
                                 allowInvalidReissueInSameBlockUntilTimestamp: Long,
                                 allowMultipleLeaseCancelTransactionUntilTimestamp: Long,
                                 resetEffectiveBalancesAtHeight: Int,
                                 blockVersion3AfterHeight: Int,
                                 preActivatedFeatures: Map[Short, Int],
                                 doubleFeaturesPeriodsAfterHeight: Int,
                                 maxTransactionTimeBackOffset: FiniteDuration,
                                 maxTransactionTimeForwardOffset: FiniteDuration) {
  val allowLeasedBalanceTransferUntilHeight: Int = blockVersion3AfterHeight

  require(featureCheckBlocksPeriod > 0, "featureCheckBlocksPeriod must be greater than 0")
  require(
    (blocksForFeatureActivation > 0) && (blocksForFeatureActivation <= featureCheckBlocksPeriod),
    s"blocksForFeatureActivation must be in range 1 to $featureCheckBlocksPeriod"
  )

  def activationWindowSize(height: Int): Int =
    featureCheckBlocksPeriod * (if (height <= doubleFeaturesPeriodsAfterHeight) 1 else 2)

  def activationWindow(height: Int): Range =
    if (height < 1) Range(0, 0)
    else {
      val ws = activationWindowSize(height)
      Range.inclusive((height - 1) / ws * ws + 1, ((height - 1) / ws + 1) * ws)
    }

  def blocksForFeatureActivation(height: Int): Int =
    blocksForFeatureActivation * (if (height <= doubleFeaturesPeriodsAfterHeight) 1 else 2)

  def generatingBalanceDepth(height: Int): Int =
    if (height >= generationBalanceDepthFrom50To1000AfterHeight) 1000 else 50
}

object FunctionalitySettings {
  val MAINNET: FunctionalitySettings = apply(
    featureCheckBlocksPeriod = 5000,
    blocksForFeatureActivation = 5000,
    allowTemporaryNegativeUntil = 0,
    generationBalanceDepthFrom50To1000AfterHeight = 0,
    minimalGeneratingBalanceAfter = 0,
    allowTransactionsFromFutureUntil = 0,
    allowUnissuedAssetsUntil = 0,
    allowInvalidReissueInSameBlockUntilTimestamp = 0,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 0,
    resetEffectiveBalancesAtHeight = 1,
    blockVersion3AfterHeight = 0,
    preActivatedFeatures = Map(
      BlockchainFeatures.SmallerMinimalGeneratingBalance.id -> 0,
      BlockchainFeatures.NG.id                              -> 0,
      BlockchainFeatures.MassTransfer.id                    -> 0,
      BlockchainFeatures.SmartAccounts.id                   -> 0,
      BlockchainFeatures.DataTransaction.id                 -> 0,
      BlockchainFeatures.BurnAnyTokens.id                   -> 0,
      BlockchainFeatures.FeeSponsorship.id                  -> 0,
      BlockchainFeatures.FairPoS.id                         -> 0
    ),
    doubleFeaturesPeriodsAfterHeight = 0,
    maxTransactionTimeBackOffset = 120.minutes,
    maxTransactionTimeForwardOffset = 90.minutes
  )

  val TESTNET: FunctionalitySettings = apply(
    featureCheckBlocksPeriod = 180,
    blocksForFeatureActivation = 180,
    allowTemporaryNegativeUntil = 0,
    generationBalanceDepthFrom50To1000AfterHeight = 0,
    minimalGeneratingBalanceAfter = 0,
    allowTransactionsFromFutureUntil = 0,
    allowUnissuedAssetsUntil = 0,
    allowInvalidReissueInSameBlockUntilTimestamp = 0,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 0,
    resetEffectiveBalancesAtHeight = 1,
    blockVersion3AfterHeight = 0,
    preActivatedFeatures = Map(
      BlockchainFeatures.SmallerMinimalGeneratingBalance.id -> 0,
      BlockchainFeatures.NG.id                              -> 0,
      BlockchainFeatures.MassTransfer.id                    -> 0,
      BlockchainFeatures.SmartAccounts.id                   -> 0,
      BlockchainFeatures.DataTransaction.id                 -> 0,
      BlockchainFeatures.BurnAnyTokens.id                   -> 0,
      BlockchainFeatures.FeeSponsorship.id                  -> 0,
      BlockchainFeatures.FairPoS.id                         -> 0,
      BlockchainFeatures.SmartAssets.id                     -> 0,
      BlockchainFeatures.SmartAccountTrading.id             -> 0,
      BlockchainFeatures.Ride4DApps.id                      -> 0,
      BlockchainFeatures.OrderV3.id                         -> 0,
      BlockchainFeatures.ReduceNFTFee.id                    -> 0
    ),
    doubleFeaturesPeriodsAfterHeight = 0,
    maxTransactionTimeBackOffset = 120.minutes,
    maxTransactionTimeForwardOffset = 90.minutes
  )

  val configPath = "acryl.blockchain.custom.functionality"
}

case class GenesisTransactionSettings(recipient: String, amount: Long)

case class GenesisSettings(blockTimestamp: Long,
                           timestamp: Long,
                           initialBalance: Long,
                           signature: Option[ByteStr],
                           transactions: Seq[GenesisTransactionSettings],
                           initialBaseTarget: Long,
                           averageBlockDelay: FiniteDuration)

object GenesisSettings {
  val MAINNET: GenesisSettings = GenesisSettings(
    1547912728383L,
    1547912728383L,
    Constants.UnitsInWave * Constants.TotalAcryl,
    ByteStr.decodeBase58("4h2W2UqpguiH4NZBBbJoJmD28UZp9aun65prZHU8gcyeQAaPpZUn8aH2mgLK4EoSoK1tMHE1atBHqZiz4XtLm2Lm").toOption,
    List(
      GenesisTransactionSettings("3EJNm7sBs4Xif1RYNbgN4ehXmXmH49x95X3", Constants.UnitsInWave * Constants.TotalAcryl),
    ),
    153722867L,
    60.seconds
  )

  val TESTNET: GenesisSettings = GenesisSettings(
    1542454288000L,
    1542454288000L,
    Constants.UnitsInWave * Constants.TotalAcryl,
    ByteStr.decodeBase58("rG7eFpSARgxUTQto5p5CLY81zxQKpTfEmf9LAsXVBx29mTPK9HyeqRWRgKHj3ebTVsLr7njy2PRwf1kvRsyKpuT").toOption,
    List(
      GenesisTransactionSettings("3JRnNWu4ubdf9PtZdSzUo4Emp4CwMRn68dF", (Constants.UnitsInWave * Constants.TotalAcryl * 0.5).toLong),
      GenesisTransactionSettings("3JCwZJGvPsdw4QCoLyN7qangAmJT7N2BTkE", (Constants.UnitsInWave * Constants.TotalAcryl * 0.5).toLong),
    ),
    153722867L,
    60.seconds
  )
}

case class BlockchainSettings(addressSchemeCharacter: Char,
                              functionalitySettings: FunctionalitySettings,
                              genesisSettings: GenesisSettings,
                              rewardsSettings: RewardsSettings)

object BlockchainType extends Enumeration {
  val TESTNET: BlockchainType.Value = Value("TESTNET")
  val MAINNET: BlockchainType.Value = Value("MAINNET")
  val CUSTOM: BlockchainType.Value  = Value("CUSTOM")
}

object BlockchainSettings {
  implicit val valueReader: ValueReader[BlockchainSettings] =
    (cfg: Config, path: String) => fromConfig(cfg.getConfig(path))

  // @deprecated("Use config.as[BlockchainSettings]", "0.17.0")
  def fromRootConfig(config: Config): BlockchainSettings = config.as[BlockchainSettings]("acryl.blockchain")

  private[this] def fromConfig(config: Config): BlockchainSettings = {
    val blockchainType = config.as[BlockchainType.Value]("type")
    val (addressSchemeCharacter: Char,
         functionalitySettings: FunctionalitySettings,
         genesisSettings: GenesisSettings,
         rewardsSettings: RewardsSettings) = blockchainType match {
      case BlockchainType.TESTNET =>
        ('K', FunctionalitySettings.TESTNET, GenesisSettings.TESTNET, RewardsSettings.TESTNET)
      case BlockchainType.MAINNET =>
        ('A', FunctionalitySettings.MAINNET, GenesisSettings.MAINNET, RewardsSettings.MAINNET)
      case BlockchainType.CUSTOM =>
        val addressSchemeCharacter = config.as[String](s"custom.address-scheme-character").charAt(0)
        val functionalitySettings  = config.as[FunctionalitySettings](s"custom.functionality")
        val genesisSettings        = config.as[GenesisSettings](s"custom.genesis")
        val rewardsSettings        = config.as[RewardsSettings](s"custom.rewards")
        (addressSchemeCharacter, functionalitySettings, genesisSettings, rewardsSettings)
    }

    BlockchainSettings(
      addressSchemeCharacter = addressSchemeCharacter,
      functionalitySettings = functionalitySettings,
      genesisSettings = genesisSettings,
      rewardsSettings = rewardsSettings
    )
  }
}
