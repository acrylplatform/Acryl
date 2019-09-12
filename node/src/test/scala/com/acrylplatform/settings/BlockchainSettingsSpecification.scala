package com.acrylplatform.settings

import com.typesafe.config.ConfigFactory
import com.acrylplatform.common.state.ByteStr
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class BlockchainSettingsSpecification extends FlatSpec with Matchers {
  "BlockchainSettings" should "read custom values" in {
    val config   = loadConfig(ConfigFactory.parseString("""acryl {
        |  directory = "/acryl"
        |  data-directory = "/acryl/data"
        |  blockchain {
        |    type = CUSTOM
        |    custom {
        |      address-scheme-character = "C"
        |      functionality {
        |        feature-check-blocks-period = 10000
        |        blocks-for-feature-activation = 9000
        |        allow-temporary-negative-until = 1
        |        generation-balance-depth-from-50-to-1000-after-height = 4
        |        minimal-generating-balance-after = 5
        |        allow-transactions-from-future-until = 6
        |        allow-unissued-assets-until = 7
        |        allow-invalid-reissue-in-same-block-until-timestamp = 12
        |        allow-multiple-lease-cancel-transaction-until-timestamp = 14
        |        reset-effective-balances-at-height = 15
        |        block-version-3-after-height = 18
        |        pre-activated-features {
        |          19 = 100
        |          20 = 200
        |        }
        |        double-features-periods-after-height = 21
        |        max-transaction-time-back-offset = 55s
        |        max-transaction-time-forward-offset = 12d
        |      }
        |      genesis {
        |        timestamp = 1460678400000
        |        block-timestamp = 1460678400000
        |        signature = "BASE58BLKSGNATURE"
        |        initial-balance = 100000000000000
        |        initial-base-target = 153722867
        |        average-block-delay = 60s
        |        transactions = [
        |          {recipient = "BASE58ADDRESS1", amount = 50000000000001},
        |          {recipient = "BASE58ADDRESS2", amount = 49999999999999}
        |        ]
        |      }
        |    }
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromRootConfig(config)

    settings.addressSchemeCharacter should be('C')
    settings.functionalitySettings.featureCheckBlocksPeriod should be(10000)
    settings.functionalitySettings.blocksForFeatureActivation should be(9000)
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(1)
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(4)
    settings.functionalitySettings.minimalGeneratingBalanceAfter should be(5)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(6)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(7)
    settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp should be(12)
    settings.functionalitySettings.allowMultipleLeaseCancelTransactionUntilTimestamp should be(14)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(15)
    settings.functionalitySettings.blockVersion3AfterHeight should be(18)
    settings.functionalitySettings.preActivatedFeatures should be(Map(19 -> 100, 20 -> 200))
    settings.functionalitySettings.doubleFeaturesPeriodsAfterHeight should be(21)
    settings.functionalitySettings.maxTransactionTimeBackOffset should be(55.seconds)
    settings.functionalitySettings.maxTransactionTimeForwardOffset should be(12.days)
    settings.genesisSettings.blockTimestamp should be(1460678400000L)
    settings.genesisSettings.timestamp should be(1460678400000L)
    settings.genesisSettings.signature should be(ByteStr.decodeBase58("BASE58BLKSGNATURE").toOption)
    settings.genesisSettings.initialBalance should be(100000000000000L)
    settings.genesisSettings.initialBaseTarget should be(153722867)
    settings.genesisSettings.averageBlockDelay should be(60.seconds)
    settings.genesisSettings.transactions should be(
      Seq(GenesisTransactionSettings("BASE58ADDRESS1", 50000000000001L), GenesisTransactionSettings("BASE58ADDRESS2", 49999999999999L)))
  }

  it should "read testnet settings" in {
    val config   = loadConfig(ConfigFactory.parseString("""acryl {
        |  directory = "/acryl"
        |  data-directory = "/acryl/data"
        |  blockchain {
        |    type = TESTNET
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromRootConfig(config)

    settings.addressSchemeCharacter should be('K')
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(0)
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(0)
    settings.functionalitySettings.minimalGeneratingBalanceAfter should be(0)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(0)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(0)
    settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp should be(0)
    settings.functionalitySettings.allowMultipleLeaseCancelTransactionUntilTimestamp should be(0)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(1)
    settings.functionalitySettings.blockVersion3AfterHeight should be(0)
    settings.functionalitySettings.maxTransactionTimeBackOffset should be(120.minutes)
    settings.functionalitySettings.maxTransactionTimeForwardOffset should be(90.minutes)
    settings.genesisSettings.blockTimestamp should be(1542454288000L)
    settings.genesisSettings.timestamp should be(1542454288000L)
    settings.genesisSettings.signature should be(
      ByteStr.decodeBase58("rG7eFpSARgxUTQto5p5CLY81zxQKpTfEmf9LAsXVBx29mTPK9HyeqRWRgKHj3ebTVsLr7njy2PRwf1kvRsyKpuT").toOption)
    settings.genesisSettings.initialBalance should be(10000000000000000L)

    settings.genesisSettings.transactions should be(
      Seq(
        GenesisTransactionSettings("3JRnNWu4ubdf9PtZdSzUo4Emp4CwMRn68dF", 5000000000000000L),
        GenesisTransactionSettings("3JCwZJGvPsdw4QCoLyN7qangAmJT7N2BTkE", 5000000000000000L)
      ))
  }

  it should "read mainnet settings" in {
    val config   = loadConfig(ConfigFactory.parseString("""acryl {
        |  directory = "/acryl"
        |  data-directory = "/acryl/data"
        |  blockchain {
        |    type = MAINNET
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromRootConfig(config)

    settings.addressSchemeCharacter should be('A')
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(0)
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(0)
    settings.functionalitySettings.minimalGeneratingBalanceAfter should be(0)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(0)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(0)
    settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp should be(0)
    settings.functionalitySettings.allowMultipleLeaseCancelTransactionUntilTimestamp should be(0)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(1)
    settings.functionalitySettings.maxTransactionTimeBackOffset should be(120.minutes)
    settings.functionalitySettings.maxTransactionTimeForwardOffset should be(90.minutes)
    settings.genesisSettings.blockTimestamp should be(1547912728383L)
    settings.genesisSettings.timestamp should be(1547912728383L)
    settings.genesisSettings.signature should be(
      ByteStr.decodeBase58("4h2W2UqpguiH4NZBBbJoJmD28UZp9aun65prZHU8gcyeQAaPpZUn8aH2mgLK4EoSoK1tMHE1atBHqZiz4XtLm2Lm").toOption)
    settings.genesisSettings.initialBalance should be(10000000000000000L)
    settings.genesisSettings.transactions should be(
      Seq(
        GenesisTransactionSettings("3EJNm7sBs4Xif1RYNbgN4ehXmXmH49x95X3", 10000000000000000L)
      ))
  }
}
