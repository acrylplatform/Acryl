package com.acrylplatform.generator
import cats.Show
import com.acrylplatform.account.KeyPair
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.crypto
import com.acrylplatform.generator.utils.Gen
import com.acrylplatform.it.util._
import com.acrylplatform.lang.script.Script
import com.acrylplatform.transaction.Asset.Acryl
import com.acrylplatform.transaction.smart.SetScriptTransaction
import com.acrylplatform.transaction.transfer.TransferTransactionV2
import com.acrylplatform.transaction.{Proofs, Transaction}

import scala.util.Random

class MultisigTransactionGenerator(settings: MultisigTransactionGenerator.Settings, val accounts: Seq[KeyPair])
    extends TransactionGenerator {

  override def next(): Iterator[Transaction] = {
    generate(settings).toIterator
  }

  private def generate(settings: MultisigTransactionGenerator.Settings): Seq[Transaction] = {

    val bank   = accounts.head
    val owners = Seq(createAccount(), accounts(1), createAccount(), accounts(2), createAccount(), accounts(3), createAccount(), createAccount())

    val enoughFee               = 0.005.acryl
    val totalAmountOnNewAccount = 1.acryl

    val script: Script = Gen.multiSigScript(owners, 3)

    val now       = System.currentTimeMillis()
    val setScript = SetScriptTransaction.selfSigned(bank, Some(script), enoughFee, now).explicitGet()

    val res = Range(0, settings.transactions).map { i =>
      val tx = TransferTransactionV2
        .create(Acryl, bank, owners(1), totalAmountOnNewAccount - 2 * enoughFee - i, now + i, Acryl, enoughFee, Array.emptyByteArray, Proofs.empty)
        .explicitGet()
      val signatures = owners.map(crypto.sign(_, tx.bodyBytes())).map(ByteStr(_))
      tx.copy(proofs = Proofs(signatures))
    }

    println(System.currentTimeMillis())
    println(s"${res.length} tx generated")

    if (settings.firstRun) setScript +: res
    else res
  }

  private def createAccount() = {
    val seedBytes = Array.fill(32)(0: Byte)
    Random.nextBytes(seedBytes)
    KeyPair(seedBytes)
  }
}

object MultisigTransactionGenerator {
  final case class Settings(transactions: Int, firstRun: Boolean)

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      s"""
        | transactions = ${x.transactions}
        | firstRun = ${x.firstRun}
      """.stripMargin
    }
  }
}
