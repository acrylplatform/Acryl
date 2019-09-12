package com.acrylplatform.account

import com.google.common.collect.Interners
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.Base58
import com.acrylplatform.crypto._
import com.acrylplatform.transaction.TxValidationError.InvalidAddress
import com.acrylplatform.utils.base58Length
import play.api.libs.json.{Format, Writes}
import supertagged._

object PublicKey extends TaggedType[ByteStr] {
  private[this] val interner = Interners.newWeakInterner[PublicKey]()

  val KeyStringLength: Int = base58Length(KeyLength)

  val empty = apply(ByteStr.empty)

  def apply(publicKey: ByteStr): PublicKey =
    interner.intern(publicKey @@ this)

  def apply(publicKey: Array[Byte]): PublicKey =
    apply(ByteStr(publicKey))

  def fromBase58String(base58: String): Either[InvalidAddress, PublicKey] =
    (for {
      _     <- Either.cond(base58.length <= KeyStringLength, (), "Bad public key string length")
      bytes <- Base58.tryDecodeWithLimit(base58).toEither.left.map(ex => s"Unable to decode base58: ${ex.getMessage}")
    } yield PublicKey(bytes)).left.map(err => InvalidAddress(s"Invalid sender: $err"))

  def unapply(arg: Array[Byte]): Option[PublicKey] =
    Some(apply(arg))

  implicit def toAddress(pk: PublicKey): Address =
    pk.toAddress

  implicit class PublicKeyImplicitOps(private val pk: PublicKey) extends AnyVal {
    def toAddress: Address = Address.fromPublicKey(pk)
  }

  implicit lazy val jsonFormat: Format[PublicKey] = Format[PublicKey](
    com.acrylplatform.utils.byteStrWrites.map(this.apply),
    Writes(pk => com.acrylplatform.utils.byteStrWrites.writes(pk))
  )
}
