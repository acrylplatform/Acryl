package com.acrylplatform.transaction.lease

import cats.implicits._
import com.google.common.primitives.Bytes
import com.acrylplatform.account.{AddressOrAlias, KeyPair, PrivateKey, PublicKey}
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.crypto
import com.acrylplatform.lang.ValidationError
import com.acrylplatform.transaction._
import com.acrylplatform.transaction.description._
import monix.eval.Coeval

import scala.util.Try

case class LeaseTransactionV1 private (sender: PublicKey, amount: Long, fee: Long, timestamp: Long, recipient: AddressOrAlias, signature: ByteStr)
    extends LeaseTransaction
    with SignedTransaction
    with FastHashId {

  override val builder: TransactionParser = LeaseTransactionV1
  val bodyBytes: Coeval[Array[Byte]]      = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), bytesBase()))
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(bodyBytes(), signature.arr))

  override def version: Byte = 1
}

object LeaseTransactionV1 extends TransactionParserFor[LeaseTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = LeaseTransaction.typeId

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      LeaseTransaction
        .validateLeaseParams(tx)
        .map(_ => tx)
        .foldToTry
    }
  }

  def create(sender: PublicKey,
             amount: Long,
             fee: Long,
             timestamp: Long,
             recipient: AddressOrAlias,
             signature: ByteStr): Either[ValidationError, TransactionT] = {
    LeaseTransaction
      .validateLeaseParams(amount, fee, recipient, sender)
      .map(_ => LeaseTransactionV1(sender, amount, fee, timestamp, recipient, signature))
  }

  def signed(sender: PublicKey,
             amount: Long,
             fee: Long,
             timestamp: Long,
             recipient: AddressOrAlias,
             signer: PrivateKey): Either[ValidationError, TransactionT] = {
    create(sender, amount, fee, timestamp, recipient, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(signer, unsigned.bodyBytes())))
    }
  }

  def selfSigned(sender: KeyPair, amount: Long, fee: Long, timestamp: Long, recipient: AddressOrAlias): Either[ValidationError, TransactionT] = {
    signed(sender, amount, fee, timestamp, recipient, sender)
  }

  val byteTailDescription: ByteEntity[LeaseTransactionV1] = {
    (
      PublicKeyBytes(tailIndex(1), "Sender's public key"),
      AddressOrAliasBytes(tailIndex(2), "Recipient"),
      LongBytes(tailIndex(3), "Amount"),
      LongBytes(tailIndex(4), "Fee"),
      LongBytes(tailIndex(5), "Timestamp"),
      SignatureBytes(tailIndex(6), "Signature")
    ) mapN {
      case (sender, recipient, amount, fee, timestamp, signature) =>
        LeaseTransactionV1(
          sender = sender,
          amount = amount,
          fee = fee,
          timestamp = timestamp,
          recipient = recipient,
          signature = signature
        )
    }
  }
}
