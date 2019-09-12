package com.acrylplatform.account

import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.transaction.TxValidationError.GenericError
import play.api.libs.json.{Format, Writes}
import supertagged._

object PrivateKey extends TaggedType[ByteStr] {
  def apply(privateKey: ByteStr): PrivateKey = {
    privateKey @@ PrivateKey
  }

  def apply(privateKey: Array[Byte]): PrivateKey =
    apply(ByteStr(privateKey))

  def unapply(arg: Array[Byte]): Option[PrivateKey] =
    Some(apply(arg))

  @deprecated("Use KeyPair.fromSeed", "0.17.0")
  def fromSeed(seed: ByteStr): PrivateKey =
    KeyPair(seed).privateKey

  @deprecated("Use KeyPair.fromSeed", "0.17.0")
  def fromSeed(base58: String): Either[GenericError, PrivateKey] =
    KeyPair.fromSeed(base58).map(_.privateKey)

  implicit lazy val jsonFormat: Format[PrivateKey] = Format[PrivateKey](
    com.acrylplatform.utils.byteStrWrites.map(this.apply),
    Writes(pk => com.acrylplatform.utils.byteStrWrites.writes(pk))
  )
}
