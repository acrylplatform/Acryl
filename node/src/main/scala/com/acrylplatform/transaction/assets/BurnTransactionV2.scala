package com.acrylplatform.transaction.assets

import cats.implicits._
import com.google.common.primitives.Bytes
import com.acrylplatform.account.{KeyPair, PrivateKey, PublicKey}
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.crypto
import com.acrylplatform.lang.ValidationError
import com.acrylplatform.transaction.Asset.IssuedAsset
import com.acrylplatform.transaction._
import com.acrylplatform.transaction.description._
import monix.eval.Coeval

import scala.util.Try

final case class BurnTransactionV2 private (chainId: Byte,
                                            sender: PublicKey,
                                            asset: IssuedAsset,
                                            quantity: Long,
                                            fee: Long,
                                            timestamp: Long,
                                            proofs: Proofs)
    extends BurnTransaction
    with FastHashId {

  override def builder: TransactionParser = BurnTransactionV2
  override def chainByte: Option[Byte]    = Some(chainId)

  override val bodyBytes: Coeval[Array[Byte]] =
    byteBase.map(base => Bytes.concat(Array(builder.typeId, version, chainId), base))

  override val bytes: Coeval[Array[Byte]] =
    (bodyBytes, proofs.bytes)
      .mapN { case (bb, pb) => Bytes.concat(Array(0: Byte), bb, pb) }

  override def version: Byte = 2
}

object BurnTransactionV2 extends TransactionParserFor[BurnTransactionV2] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = BurnTransaction.typeId
  override val supportedVersions: Set[Byte] = Set(2)

  override protected def parseTail(bytes: Array[Byte]): Try[BurnTransactionV2] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      BurnTransaction
        .validateBurnParams(tx)
        .map(_ => tx)
        .foldToTry
    }
  }

  def create(chainId: Byte,
             sender: PublicKey,
             asset: IssuedAsset,
             quantity: Long,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, BurnTransactionV2] = {
    BurnTransaction
      .validateBurnParams(quantity, fee)
      .map(_ => BurnTransactionV2(chainId, sender, asset, quantity, fee, timestamp, proofs))
  }

  def signed(chainId: Byte,
             sender: PublicKey,
             asset: IssuedAsset,
             quantity: Long,
             fee: Long,
             timestamp: Long,
             signer: PrivateKey): Either[ValidationError, TransactionT] = {
    for {
      unsigned <- create(chainId, sender, asset, quantity, fee, timestamp, Proofs.empty)
      proofs   <- Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes()))))
    } yield unsigned.copy(proofs = proofs)
  }

  def selfSigned(chainId: Byte,
                 sender: KeyPair,
                 asset: IssuedAsset,
                 quantity: Long,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(chainId, sender, asset, quantity, fee, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[BurnTransactionV2] = {
    (
      OneByte(tailIndex(1), "Chain ID"),
      PublicKeyBytes(tailIndex(2), "Sender's public key"),
      ByteStrDefinedLength(tailIndex(3), "Asset ID", AssetIdLength).map(IssuedAsset),
      LongBytes(tailIndex(4), "Quantity"),
      LongBytes(tailIndex(5), "Fee"),
      LongBytes(tailIndex(6), "Timestamp"),
      ProofsBytes(tailIndex(7))
    ) mapN BurnTransactionV2.apply
  }
}
