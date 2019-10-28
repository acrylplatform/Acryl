package com.acrylplatform.lagonaki.unit

import java.io.File
import java.nio.file.Files

import cats.syntax.option._
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.settings.WalletSettings
import com.acrylplatform.wallet.Wallet
import org.scalatest.{FunSuite, Matchers}

class WalletSpecification extends FunSuite with Matchers {

  private val walletSize = 10
  val w                  = Wallet(WalletSettings(None, "cookies".some, ByteStr.decodeBase58("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption))

  test("wallet - acc creation") {
    w.generateNewAccounts(walletSize)

    w.privateKeyAccounts.size shouldBe walletSize
    w.privateKeyAccounts.map(_.address) shouldBe Seq(
      "3JMqe2pvyP6wugikua8WZVexRxGeSXG3HjD",
      "3JYPHoWZhW5BLRJgQMqXrNFixDSrBtrvBXS",
      "3JHu4pKUXqu82Q4YKdqat5xS8RiWRh4HRdh",
      "3JUbSvgXXk6v5xna11oviDAmuVx7vpo7q6y",
      "3JDKX5JpUXQcYsK2iJ2hG44xFTsYP74skrB",
      "3JRiDweo83WLSt2tiqQKM1qEHxwWT2Z1Brj",
      "3JHdVfLtMxHCtiY2q1uGjfNM37oCHvGQPCU",
      "3JH8VcHNdnF6G4UtxMjjna53h29FuoGTqWY",
      "3JDm6CBMssZNakiGF5csDp5YiDpsduSgwLy",
      "3JPMidWopVusPUogWfXDNcA8y8xs8J7qzWw"
    )
  }

  test("wallet - acc deletion") {

    val head = w.privateKeyAccounts.head
    w.deleteAccount(head)
    assert(w.privateKeyAccounts.lengthCompare(walletSize - 1) == 0)

    w.deleteAccount(w.privateKeyAccounts.head)
    assert(w.privateKeyAccounts.lengthCompare(walletSize - 2) == 0)

    w.privateKeyAccounts.foreach(w.deleteAccount)

    assert(w.privateKeyAccounts.isEmpty)
  }

  test("reopening") {
    val walletFile = Some(createTestTemporaryFile("wallet", ".dat"))

    val w1 = Wallet(WalletSettings(walletFile, "cookies".some, ByteStr.decodeBase58("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption))
    w1.generateNewAccounts(10)
    val w1PrivateKeys = w1.privateKeyAccounts
    val w1nonce              = w1.nonce

    val w2 = Wallet(WalletSettings(walletFile, "cookies".some, None))
    w2.privateKeyAccounts.nonEmpty shouldBe true
    w2.privateKeyAccounts shouldEqual w1PrivateKeys
    w2.nonce shouldBe w1nonce
  }

  test("reopen with incorrect password") {
    val file = Some(createTestTemporaryFile("wallet", ".dat"))
    val w1   = Wallet(WalletSettings(file, "password".some, ByteStr.decodeBase58("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").toOption))
    w1.generateNewAccounts(3)

    assertThrows[IllegalArgumentException] {
      Wallet(WalletSettings(file, "incorrect password".some, None))
    }
  }

  def createTestTemporaryFile(name: String, ext: String): File = {
    val file = Files.createTempFile(name, ext).toFile
    file.deleteOnExit()

    file
  }
}
