package com.acrylplatform.generator

import cats.Show
import com.acrylplatform.account.KeyPair
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.generator.OracleTransactionGenerator.Settings
import com.acrylplatform.generator.utils.Gen
import com.acrylplatform.it.util._
import com.acrylplatform.state._
import com.acrylplatform.transaction.Asset.Acryl
import com.acrylplatform.transaction.smart.SetScriptTransaction
import com.acrylplatform.transaction.transfer.TransferTransactionV2
import com.acrylplatform.transaction.{DataTransaction, Transaction}

class OracleTransactionGenerator(settings: Settings, val accounts: Seq[KeyPair]) extends TransactionGenerator {
  override def next(): Iterator[Transaction] = generate(settings).toIterator

  def generate(settings: Settings): Seq[Transaction] = {
    val oracle = accounts.last

    val scriptedAccount = accounts.head

    val script = Gen.oracleScript(oracle, settings.requiredData)

    val enoughFee = 0.005.acryl

    val setScript: Transaction =
      SetScriptTransaction
        .selfSigned(scriptedAccount, Some(script), enoughFee, System.currentTimeMillis())
        .explicitGet()

    val setDataTx: Transaction = DataTransaction
      .selfSigned(oracle, settings.requiredData.toList, enoughFee, System.currentTimeMillis())
      .explicitGet()

    val now = System.currentTimeMillis()
    val transactions: List[Transaction] = (1 to settings.transactions).map { i =>
      TransferTransactionV2
        .selfSigned(Acryl, scriptedAccount, oracle, 1.acryl, now + i, Acryl, enoughFee, Array.emptyByteArray)
        .explicitGet()
    }.toList

    setScript +: setDataTx +: transactions
  }
}

object OracleTransactionGenerator {
  final case class Settings(transactions: Int, requiredData: Set[DataEntry[_]])

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      s"Transactions: ${x.transactions}\n" +
        s"DataEntries: ${x.requiredData}\n"
    }
  }
}
