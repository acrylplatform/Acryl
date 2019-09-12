package com.acrylplatform.transaction

import com.google.protobuf.ByteString
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.Base58
import com.acrylplatform.protobuf.transaction.AssetId
import com.acrylplatform.transaction.assets.exchange.AssetPair
import net.ceedubs.ficus.readers.ValueReader
import play.api.libs.json._

import scala.util.Success

sealed trait Asset
object Asset {
  final case class IssuedAsset(id: ByteStr) extends Asset
  case object Acryl                         extends Asset

  implicit val assetReads: Reads[IssuedAsset] = Reads {
    case JsString(str) if str.length > AssetIdStringLength => JsError("invalid.feeAssetId")
    case JsString(str) =>
      Base58.tryDecodeWithLimit(str) match {
        case Success(arr) => JsSuccess(IssuedAsset(ByteStr(arr)))
        case _            => JsError("Expected base58-encoded assetId")
      }
    case _ => JsError("Expected base58-encoded assetId")
  }
  implicit val assetWrites: Writes[IssuedAsset] = Writes { asset =>
    JsString(asset.id.base58)
  }

  implicit val assetIdReads: Reads[Asset] = Reads {
    case json: JsString => assetReads.reads(json)
    case JsNull         => JsSuccess(Acryl)
    case _              => JsError("Expected base58-encoded assetId or null")
  }
  implicit val assetIdWrites: Writes[Asset] = Writes {
    case Acryl           => JsNull
    case IssuedAsset(id) => JsString(id.base58)
  }

  implicit val assetJsonFormat: Format[IssuedAsset] = Format(assetReads, assetWrites)
  implicit val assetIdJsonFormat: Format[Asset]     = Format(assetIdReads, assetIdWrites)

  implicit val assetReader: ValueReader[Asset] = { (cfg, path) =>
    AssetPair.extractAssetId(cfg getString path).fold(ex => throw new Exception(ex.getMessage), identity)
  }

  def fromString(maybeStr: Option[String]): Asset = {
    maybeStr.map(x => IssuedAsset(ByteStr.decodeBase58(x).get)).getOrElse(Acryl)
  }

  def fromCompatId(maybeBStr: Option[ByteStr]): Asset = {
    maybeBStr.fold[Asset](Acryl)(IssuedAsset)
  }

  def fromProtoId(byteStr: ByteString): Asset = {
    if (byteStr.isEmpty) Acryl
    else IssuedAsset(byteStr.toByteArray)
  }

  def fromProtoId(assetId: AssetId): Asset = assetId.asset match {
    case AssetId.Asset.IssuedAsset(bs) => fromProtoId(bs)
    case _ => Acryl
  }

  implicit class AssetIdOps(private val ai: Asset) extends AnyVal {
    def byteRepr: Array[Byte] = ai match {
      case Acryl           => Array(0: Byte)
      case IssuedAsset(id) => (1: Byte) +: id.arr
    }

    def protoId: AssetId = ai match {
      case IssuedAsset(id) => AssetId().withIssuedAsset(ByteString.copyFrom(id))
      case Acryl => AssetId().withAcryl(com.google.protobuf.empty.Empty())
    }

    def compatId: Option[ByteStr] = ai match {
      case Acryl           => None
      case IssuedAsset(id) => Some(id)
    }

    def maybeBase58Repr: Option[String] = ai match {
      case Acryl           => None
      case IssuedAsset(id) => Some(id.base58)
    }

    def fold[A](onAcryl: => A)(onAsset: IssuedAsset => A): A = ai match {
      case Acryl                  => onAcryl
      case asset @ IssuedAsset(_) => onAsset(asset)
    }
  }
}
