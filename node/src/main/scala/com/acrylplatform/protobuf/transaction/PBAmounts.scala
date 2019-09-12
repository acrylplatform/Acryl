package com.acrylplatform.protobuf.transaction
import com.google.protobuf.ByteString
import com.acrylplatform.transaction.Asset
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}

object PBAmounts {
  def toPBAssetId(asset: Asset): AssetId = asset match {
    case Asset.IssuedAsset(id) =>
      AssetId().withIssuedAsset(ByteString.copyFrom(id))

    case Asset.Acryl =>
      AssetId().withAcryl(com.google.protobuf.empty.Empty())
  }

  def toVanillaAssetId(assetId: AssetId): Asset = assetId.asset match {
    case AssetId.Asset.Acryl(_)             => Acryl
    case AssetId.Asset.IssuedAsset(assetId) => IssuedAsset(assetId.toByteArray)
    case _ => throw new IllegalArgumentException
  }

  def fromAssetAndAmount(asset: Asset, amount: Long): Amount =
    Amount(Some(toPBAssetId(asset)), amount)

  def toAssetAndAmount(value: Amount): (Asset, Long) =
    (toVanillaAssetId(value.getAssetId), value.amount)
}
