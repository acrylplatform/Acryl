package com.acrylplatform.consensus.nxt

import com.acrylplatform.account.{KeyPair, Address}
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.consensus.TransactionsOrdering
import com.acrylplatform.transaction.Asset
import com.acrylplatform.transaction.Asset.Acryl
import com.acrylplatform.transaction.transfer._
import org.scalatest.{Assertions, Matchers, PropSpec}

import scala.util.Random

class TransactionsOrderingSpecification extends PropSpec with Assertions with Matchers {

  property("TransactionsOrdering.InBlock should sort correctly") {
    val correctSeq = Seq(
      TransferTransactionV1
        .selfSigned(
          Acryl,
          KeyPair(Array.fill(32)(0: Byte)),
          Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet(),
          100000,
          1,
          Acryl,
          125L,
          Array.empty
        )
        .right
        .get,
      TransferTransactionV1
        .selfSigned(Acryl,
                    KeyPair(Array.fill(32)(0: Byte)),
                    Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet(),
                    100000,
                    2,
                    Acryl,
                    124L,
                    Array.empty)
        .right
        .get,
      TransferTransactionV1
        .selfSigned(Acryl,
                    KeyPair(Array.fill(32)(0: Byte)),
                    Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet(),
                    100000,
                    1,
                    Acryl,
                    124L,
                    Array.empty)
        .right
        .get,
      TransferTransactionV1
        .selfSigned(
          Acryl,
          KeyPair(Array.fill(32)(0: Byte)),
          Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet(),
          100000,
          2,
          Asset.fromCompatId(Some(ByteStr.empty)),
          124L,
          Array.empty
        )
        .right
        .get,
      TransferTransactionV1
        .selfSigned(
          Acryl,
          KeyPair(Array.fill(32)(0: Byte)),
          Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet(),
          100000,
          1,
          Asset.fromCompatId(Some(ByteStr.empty)),
          124L,
          Array.empty
        )
        .right
        .get
    )

    val sorted = Random.shuffle(correctSeq).sorted(TransactionsOrdering.InBlock)

    sorted shouldBe correctSeq
  }

  property("TransactionsOrdering.InUTXPool should sort correctly") {
    val correctSeq = Seq(
      TransferTransactionV1
        .selfSigned(
          Acryl,
          KeyPair(Array.fill(32)(0: Byte)),
          Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet(),
          100000,
          1,
          Acryl,
          124L,
          Array.empty
        )
        .right
        .get,
      TransferTransactionV1
        .selfSigned(
          Acryl,
          KeyPair(Array.fill(32)(0: Byte)),
          Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet(),
          100000,
          1,
          Acryl,
          123L,
          Array.empty
        )
        .right
        .get,
      TransferTransactionV1
        .selfSigned(
          Acryl,
          KeyPair(Array.fill(32)(0: Byte)),
          Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet(),
          100000,
          2,
          Acryl,
          123L,
          Array.empty
        )
        .right
        .get,
      TransferTransactionV1
        .selfSigned(
          Acryl,
          KeyPair(Array.fill(32)(0: Byte)),
          Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet(),
          100000,
          1,
          Asset.fromCompatId(Some(ByteStr.empty)),
          124L,
          Array.empty
        )
        .right
        .get,
      TransferTransactionV1
        .selfSigned(
          Acryl,
          KeyPair(Array.fill(32)(0: Byte)),
          Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet(),
          100000,
          2,
          Asset.fromCompatId(Some(ByteStr.empty)),
          124L,
          Array.empty
        )
        .right
        .get
    )

    val sorted = Random.shuffle(correctSeq).sorted(TransactionsOrdering.InUTXPool)

    sorted shouldBe correctSeq
  }

  property("TransactionsOrdering.InBlock should sort txs by decreasing block timestamp") {
    val correctSeq = Seq(
      TransferTransactionV1
        .selfSigned(
          Acryl,
          KeyPair(Array.fill(32)(0: Byte)),
          Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet(),
          100000,
          124L,
          Acryl,
          1,
          Array()
        )
        .right
        .get,
      TransferTransactionV1
        .selfSigned(
          Acryl,
          KeyPair(Array.fill(32)(0: Byte)),
          Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet(),
          100000,
          123L,
          Acryl,
          1,
          Array()
        )
        .right
        .get
    )

    Random.shuffle(correctSeq).sorted(TransactionsOrdering.InBlock) shouldBe correctSeq
  }

  property("TransactionsOrdering.InUTXPool should sort txs by ascending block timestamp") {
    val correctSeq = Seq(
      TransferTransactionV1
        .selfSigned(
          Acryl,
          KeyPair(Array.fill(32)(0: Byte)),
          Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet(),
          100000,
          123L,
          Acryl,
          1,
          Array()
        )
        .right
        .get,
      TransferTransactionV1
        .selfSigned(
          Acryl,
          KeyPair(Array.fill(32)(0: Byte)),
          Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet(),
          100000,
          124L,
          Acryl,
          1,
          Array()
        )
        .right
        .get
    )
    Random.shuffle(correctSeq).sorted(TransactionsOrdering.InUTXPool) shouldBe correctSeq
  }
}
