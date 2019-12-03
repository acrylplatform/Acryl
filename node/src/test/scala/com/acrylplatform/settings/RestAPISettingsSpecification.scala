package com.acrylplatform.settings

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.scalatest.{FlatSpec, Matchers}

class RestAPISettingsSpecification extends FlatSpec with Matchers {
  "RestAPISettings" should "read values" in {
    val config   = ConfigFactory.parseString("""
                                               |acryl {
                                               |  rest-api {
                                               |    enable: yes
                                               |    bind-address: "127.0.0.1"
                                               |    port: 6869
                                               |    api-key-hash: "BASE58APIKEYHASH"
                                               |    cors: yes
                                               |    https: no
                                               |    https-password: "password"
                                               |    certificate-file: "/tmp/crt"
                                               |    api-key-different-host: yes
                                               |    transactions-by-address-limit = 10000
                                               |    distribution-address-limit = 10000
                                               |    allow-tx-rebroadcasting = true
                                               |    max-blocks-per-request = 100
                                               |  }
                                               |}
      """.stripMargin)
    val settings = config.as[RestAPISettings]("acryl.rest-api")

    settings.enable should be(true)
    settings.bindAddress should be("127.0.0.1")
    settings.port should be(6869)
    settings.apiKeyHash should be("BASE58APIKEYHASH")
    settings.cors should be(true)
    settings.https should be(false)
    settings.httpsPassword should be("password")
    settings.certificateFile should be("/tmp/crt")
    settings.apiKeyDifferentHost should be(true)
    settings.transactionsByAddressLimit shouldBe 10000
    settings.distributionAddressLimit shouldBe 10000
    settings.allowTxRebroadcasting shouldBe true
    settings.maxBlocksPerRequest shouldBe 100
  }
}
