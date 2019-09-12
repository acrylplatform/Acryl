package com.acrylplatform.it

import java.nio.charset.StandardCharsets

import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.state._
import com.acrylplatform.transaction.Asset
import com.acrylplatform.transaction.assets.exchange.AssetPair
import com.acrylplatform.utils.Paged
import org.asynchttpclient.Response
import play.api.libs.functional.syntax._
import play.api.libs.json.Json.parse
import play.api.libs.json.{JsError, JsString, JsSuccess, Reads, _}

import scala.concurrent.{ExecutionContext, Future}

package object api {
  implicit class ResponseFutureExt(val f: Future[Response]) extends AnyVal {
    def as[A: Reads](implicit ec: ExecutionContext): Future[A] = f.map(r => parse(r.getResponseBody(StandardCharsets.UTF_8)).as[A])(ec)
  }

  implicit val addressReads: Reads[com.acrylplatform.account.Address] = Reads {
    case JsString(addrStr) =>
      com.acrylplatform.account.Address
        .fromString(addrStr)
        .fold(err => JsError(err.toString), addr => JsSuccess(addr))
    case _ => JsError("Expected base58 encoded address")
  }

  implicit val dstMapReads: Reads[Map[com.acrylplatform.account.Address, Long]] = Reads { json =>
    json.validate[Map[String, Long]].map { dst =>
      dst.map {
        case (addrStr, balance) =>
          com.acrylplatform.account.Address.fromString(addrStr).explicitGet() -> balance
      }
    }
  }

  implicit val rateMapReads: Reads[Map[Asset, Double]] = Reads { json =>
    json.validate[Map[String, Double]].map { rate =>
      rate.map { case (assetStr, rateValue) => AssetPair.extractAssetId(assetStr).get -> rateValue }
    }
  }

  implicit val distributionReads: Reads[AssetDistribution] = Reads { json =>
    json
      .validate[Map[com.acrylplatform.account.Address, Long]]
      .map(dst => AssetDistribution(dst))
  }

  implicit def pagedReads[C: Reads, R: Reads]: Reads[Paged[C, R]] =
    (
      (JsPath \ "hasNext").read[Boolean] and
        (JsPath \ "lastItem").readNullable[C] and
        (JsPath \ "items").read[R]
    )(Paged.apply[C, R] _)

  implicit val distributionPageReads: Reads[AssetDistributionPage] = Reads { json =>
    json.validate[Paged[com.acrylplatform.account.Address, AssetDistribution]].map(pg => AssetDistributionPage(pg))
  }
}
