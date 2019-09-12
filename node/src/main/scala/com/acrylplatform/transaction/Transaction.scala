package com.acrylplatform.transaction

import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.serialization.{BytesSerializable, JsonSerializable}
import com.acrylplatform.state._
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import monix.eval.Coeval
import play.api.libs.json.Json

trait Transaction extends BytesSerializable with JsonSerializable {
  val id: Coeval[ByteStr]

  def builder: TransactionParser
  def assetFee: (Asset, Long)
  def timestamp: Long
  def chainByte: Option[Byte] = None

  override def toString: String = json().toString

  def toPrettyString: String = json.map(Json.prettyPrint).value

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id() == tx.id()
    case _               => false
  }

  override def hashCode(): Int = id().hashCode()

  val bodyBytes: Coeval[Array[Byte]]
  def checkedAssets(): Seq[IssuedAsset] = Seq.empty
}

object Transaction {

  type Type = Byte

  implicit class TransactionExt(tx: Transaction) {
    def feeDiff(): Portfolio = tx.assetFee match {
      case (asset @ IssuedAsset(_), fee) =>
        Portfolio(balance = 0, lease = LeaseBalance.empty, assets = Map(asset -> fee))
      case (Acryl, fee) => Portfolio(balance = fee, lease = LeaseBalance.empty, assets = Map.empty)
    }
  }

}
