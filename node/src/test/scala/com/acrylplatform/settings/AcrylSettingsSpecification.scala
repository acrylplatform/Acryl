package com.acrylplatform.settings

import java.io.File

import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

class AcrylSettingsSpecification extends FlatSpec with Matchers {

  private def config(configName: String) = {
    AcrylSettings.fromRootConfig(
      com.acrylplatform.settings.loadConfig(
        ConfigFactory.parseFile(new File(s"acryl-$configName.conf"))
      )
    )
  }

  def testConfig(configName: String)(additionalChecks: AcrylSettings => Unit = _ => ()) {
    "AcrylSettings" should s"read values from default config with $configName overrides" in {
      val settings = config(configName)

      val expected = ConfigFactory.parseString(s"acryl.directory = ${com.acrylplatform.settings.defaultDirectory(settings.config)}")
        .withFallback(ConfigFactory.load())
        .resolve()
        .getString("acryl.directory")

      settings.directory should be(expected)
      settings.networkSettings should not be null
      settings.walletSettings should not be null
      settings.blockchainSettings should not be null
      settings.minerSettings should not be null
      settings.restAPISettings should not be null
      settings.synchronizationSettings should not be null
      settings.utxSettings should not be null
      additionalChecks(settings)
    }
  }

  testConfig("mainnet")()
  testConfig("testnet")()
  testConfig("devnet")()

  "AcrylSettings" should "resolve folders correctly" in {
    val config = loadConfig(ConfigFactory.parseString(s"""acryl {
         |  directory = "/xxx"
         |  data-directory = "/xxx/data"
         |  ntp-server = "example.com"
         |}""".stripMargin))

    val settings = AcrylSettings.fromRootConfig(config.resolve())

    settings.directory should be("/xxx")
    settings.dbSettings.directory should be("/xxx/data")
    settings.ntpServer should be("example.com")
    settings.networkSettings.file should be(Some(new File("/xxx/peers.dat")))
    settings.walletSettings.file should be(Some(new File("/xxx/wallet/wallet.dat")))
  }

}
