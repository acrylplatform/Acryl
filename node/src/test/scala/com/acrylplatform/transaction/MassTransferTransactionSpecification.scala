package com.acrylplatform.transaction

import com.acrylplatform.TransactionGen
import com.acrylplatform.account.PublicKey
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.{Base58, EitherExt2}
import com.acrylplatform.transaction.Asset.Acryl
import com.acrylplatform.transaction.TxValidationError.GenericError
import com.acrylplatform.transaction.transfer.MassTransferTransaction.{MaxTransferCount, ParsedTransfer, Transfer}
import com.acrylplatform.transaction.transfer._
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.Json

class MassTransferTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("serialization roundtrip") {
    forAll(massTransferGen) { tx: MassTransferTransaction =>
      require(tx.bytes().head == MassTransferTransaction.typeId)
      val recovered = MassTransferTransaction.parseBytes(tx.bytes()).get

      recovered.sender.address shouldEqual tx.sender.address
      recovered.assetId shouldBe tx.assetId
      recovered.timestamp shouldEqual tx.timestamp
      recovered.fee shouldEqual tx.fee

      recovered.transfers.zip(tx.transfers).foreach {
        case (ParsedTransfer(rr, ra), ParsedTransfer(tr, ta)) =>
          rr shouldEqual tr
          ra shouldEqual ta
      }

      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("serialization from TypedTransaction") {
    forAll(massTransferGen) { tx: MassTransferTransaction =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("property validation") {
    import MassTransferTransaction.create

    forAll(massTransferGen) {
      case MassTransferTransaction(assetId, sender, transfers, timestamp, fee, attachment, proofs) =>
        val tooManyTransfers   = List.fill(MaxTransferCount + 1)(ParsedTransfer(sender.toAddress, 1L))
        val tooManyTransfersEi = create(assetId, sender, tooManyTransfers, timestamp, fee, attachment, proofs)
        tooManyTransfersEi shouldBe Left(GenericError(s"Number of transfers ${tooManyTransfers.length} is greater than $MaxTransferCount"))

        val negativeTransfer   = List(ParsedTransfer(sender.toAddress, -1L))
        val negativeTransferEi = create(assetId, sender, negativeTransfer, timestamp, fee, attachment, proofs)
        negativeTransferEi shouldBe Left(GenericError("One of the transfers has negative amount"))

        val oneHalf    = Long.MaxValue / 2 + 1
        val overflow   = List.fill(2)(ParsedTransfer(sender.toAddress, oneHalf))
        val overflowEi = create(assetId, sender, overflow, timestamp, fee, attachment, proofs)
        overflowEi shouldBe Left(TxValidationError.OverflowError)

        val feeOverflow   = List(ParsedTransfer(sender.toAddress, oneHalf))
        val feeOverflowEi = create(assetId, sender, feeOverflow, timestamp, oneHalf, attachment, proofs)
        feeOverflowEi shouldBe Left(TxValidationError.OverflowError)

        val longAttachment   = Array.fill(TransferTransaction.MaxAttachmentSize + 1)(1: Byte)
        val longAttachmentEi = create(assetId, sender, transfers, timestamp, fee, longAttachment, proofs)
        longAttachmentEi shouldBe Left(TxValidationError.TooBigArray)

        val noFeeEi = create(assetId, sender, feeOverflow, timestamp, 0, attachment, proofs)
        noFeeEi shouldBe Left(TxValidationError.InsufficientFee())

        val negativeFeeEi = create(assetId, sender, feeOverflow, timestamp, -100, attachment, proofs)
        negativeFeeEi shouldBe Left(TxValidationError.InsufficientFee())
    }
  }

  property(testName = "JSON format validation") {
    val js = Json.parse("""{
                       "type": 11,
                       "id": "CGhcg6UyfpKLRQuaayxZRmsB6UdwiDFRsuyDeuTAA9k1",
                       "sender": "3JTDzz1XbK7KeRJXGqpaRFraC92ebStimJ9",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 200000,
                       "feeAssetId": null,
                       "timestamp": 1518091313964,
                       "proofs": [
                       "FXMNu3ecy5zBjn9b69VtpuYRwxjCbxdkZ3xZpLzB8ZeFDvcgTkmEDrD29wtGYRPtyLS3LPYrL2d5UM6TpFBMUGQ"],
                       "version": 1,
                       "assetId": null,
                       "attachment": "59QuUcqP6p",
                       "transferCount": 2,
                       "totalAmount": 300000000,
                       "transfers": [
                       {
                       "recipient": "3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB",
                       "amount": 100000000
                       },
                       {
                       "recipient": "3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB",
                       "amount": 200000000
                       }
                       ]
                       }
  """)

    val transfers = MassTransferTransaction
      .parseTransfersList(
        List(Transfer("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB", 100000000L), Transfer("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB", 200000000L)))
      .right
      .get

    val tx = MassTransferTransaction
      .create(
        Acryl,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        transfers,
        1518091313964L,
        200000,
        Base58.tryDecodeWithLimit("59QuUcqP6p").get,
        Proofs(Seq(ByteStr.decodeBase58("FXMNu3ecy5zBjn9b69VtpuYRwxjCbxdkZ3xZpLzB8ZeFDvcgTkmEDrD29wtGYRPtyLS3LPYrL2d5UM6TpFBMUGQ").get))
      )
      .right
      .get

    js shouldEqual tx.json()
  }
}
