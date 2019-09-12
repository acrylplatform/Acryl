package com.acrylplatform.state
import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.acrylplatform.account.Address
import com.acrylplatform.common.utils._
import com.acrylplatform.protobuf.transaction.{PBAmounts, PBTransactions, InvokeScriptResult => PBInvokeScriptResult}
import com.acrylplatform.protobuf.utils.PBUtils
import com.acrylplatform.transaction.Asset
import com.acrylplatform.transaction.Asset.Acryl
import play.api.libs.json.Json

final case class InvokeScriptResult(data: Seq[DataEntry[_]] = Nil, transfers: Seq[InvokeScriptResult.Payment] = Nil)

//noinspection TypeAnnotation
object InvokeScriptResult {
  val empty = InvokeScriptResult()

  final case class Payment(address: Address, asset: Asset, amount: Long)
  object Payment {
    implicit val jsonFormat = Json.format[Payment]
  }

  def paymentsFromPortfolio(addr: Address, portfolio: Portfolio): Seq[Payment] = {
    val acryl  = InvokeScriptResult.Payment(addr, Acryl, portfolio.balance)
    val assets = portfolio.assets.map { case (assetId, amount) => InvokeScriptResult.Payment(addr, assetId, amount) }
    (assets.toVector ++ Some(acryl)).filter(_.amount != 0)
  }

  implicit val jsonFormat = Json.format[InvokeScriptResult]

  implicit val monoid = new Monoid[InvokeScriptResult] {
    override val empty: InvokeScriptResult =
      InvokeScriptResult.this.empty

    override def combine(x: InvokeScriptResult, y: InvokeScriptResult): InvokeScriptResult =
      InvokeScriptResult(x.data ++ y.data, x.transfers ++ y.transfers)
  }

  def toBytes(isr: InvokeScriptResult): Array[Byte] = {
    val pbValue = this.toPB(isr)
    PBUtils.encodeDeterministic(pbValue)
  }

  def fromBytes(bs: Array[Byte]): InvokeScriptResult = {
    val pbValue = PBInvokeScriptResult.parseFrom(bs)
    fromPB(pbValue)
  }

  def toPB(isr: InvokeScriptResult): PBInvokeScriptResult = {
    PBInvokeScriptResult(
      isr.data.map(PBTransactions.toPBDataEntry),
      isr.transfers.map(
        payment =>
          PBInvokeScriptResult.Payment(
            ByteString.copyFrom(payment.address.bytes),
            Some(PBAmounts.fromAssetAndAmount(payment.asset, payment.amount))
          ))
    )
  }

  def fromPB(pbValue: PBInvokeScriptResult): InvokeScriptResult = {
    InvokeScriptResult(
      pbValue.data.map(PBTransactions.toVanillaDataEntry),
      pbValue.transfers.map { p =>
        val (asset, amount) = PBAmounts.toAssetAndAmount(p.getAmount)
        InvokeScriptResult.Payment(Address.fromBytes(p.address.toByteArray).explicitGet(), asset, amount)
      }
    )
  }
}
