package com.acrylplatform.http

import com.acrylplatform.RequestGen
import com.acrylplatform.api.http.ApiError._
import com.acrylplatform.api.http._
import com.acrylplatform.api.http.alias.AliasBroadcastApiRoute
import com.acrylplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.acrylplatform.transaction.Transaction
import com.acrylplatform.transaction.TxValidationError.GenericError
import com.acrylplatform.transaction.smart.script.trace.TracedResult
import com.acrylplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import org.scalamock.scalatest.PathMockFactory
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.Json._
import play.api.libs.json._

class AliasBroadcastRouteSpec
    extends RouteSpec("/alias/broadcast/")
    with RequestGen
    with PathMockFactory
    with PropertyChecks
    with RestAPISettingsHelper {
  private val utx         = stub[UtxPool]
  private val allChannels = stub[ChannelGroup]

  (utx.putIfNew _)
    .when(*, *)
    .onCall((t: Transaction, _: Boolean) => TracedResult(Left(TransactionValidationError(GenericError("foo"), t))))
    .anyNumberOfTimes()


  "returns StateCheckFiled" - {
    val route = AliasBroadcastApiRoute(restAPISettings, utx, allChannels).route

    def posting(url: String, v: JsValue): RouteTestResult = Post(routePath(url), v) ~> route

    "when state validation fails" in {
      forAll(createAliasGen.retryUntil(_.version == 1)) { t: Transaction =>
        posting("create", t.json()) should produce(StateCheckFailed(t, "foo"))
      }
    }
  }

  "returns appropriate error code when validation fails for" - {
    val route = AliasBroadcastApiRoute(restAPISettings, utx, allChannels).route

    "create alias transaction" in forAll(createAliasReq) { req =>
      import com.acrylplatform.api.http.alias.SignedCreateAliasV1Request.broadcastAliasV1RequestReadsFormat

      def posting(v: JsValue): RouteTestResult = Post(routePath("create"), v) ~> route

      forAll(invalidBase58) { s =>
        posting(toJson(req.copy(senderPublicKey = s))) should produce(InvalidAddress)
      }
      forAll(nonPositiveLong) { q =>
        posting(toJson(req.copy(fee = q))) should produce(InsufficientFee())
      }
      forAll(invalidAliasStringByLength) { q =>
        val obj = toJson(req).as[JsObject] ++ Json.obj("alias" -> JsString(q))
        posting(obj) should produce(CustomValidationError(s"Alias '$q' length should be between 4 and 30"))
      }
    }
  }
}
