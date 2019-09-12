package com.acrylplatform.api.http.assets

import akka.http.scaladsl.server.Route
import com.acrylplatform.api.http._
import com.acrylplatform.http.BroadcastRoute
import com.acrylplatform.lang.ValidationError
import com.acrylplatform.network._
import com.acrylplatform.settings.RestAPISettings
import com.acrylplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.acrylplatform.transaction.smart.script.trace.TracedResult
import com.acrylplatform.transaction.Transaction
import com.acrylplatform.transaction.TxValidationError._
import com.acrylplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Left, Right}

case class AssetsBroadcastApiRoute(settings: RestAPISettings, utx: UtxPool, allChannels: ChannelGroup)
    extends ApiRoute
    with BroadcastRoute
    with WithSettings {

  override val route: Route = pathPrefix("assets" / "broadcast") {
    issue ~ reissue ~ transfer ~ burnRoute ~ batchTransfer ~ exchange
  }

  def issue: Route = (path("issue") & post) {
    json[SignedIssueV1Request] { issueReq =>
      doBroadcast(issueReq.toTx)
    }
  }

  def reissue: Route = (path("reissue") & post) {
    json[SignedReissueV1Request] { reissueReq =>
      doBroadcast(reissueReq.toTx)
    }
  }

  def burnRoute: Route = (path("burn") & post) {
    json[SignedBurnV1Request] { burnReq =>
      doBroadcast(burnReq.toTx)
    }
  }

  def batchTransfer: Route = (path("batch-transfer") & post) {
    json[List[SignedTransferRequests]] { reqs =>
      val r = Future
        .traverse(reqs) { req =>
          Future {
            req.eliminate(
              _.toTx,
              _.eliminate(
                _.toTx,
                _ => Left(UnsupportedTransactionType)
              )
            )
          }
        }
        .map { xs: List[Either[ValidationError, Transaction]] =>
          xs.view
            .map {
              case Left(e)   => TracedResult(Left(e))
              case Right(tx) => utx.putIfNew(tx).map { case shouldBroadcast => (tx, shouldBroadcast) }
            }
            .map {
              case TracedResult(result, log) =>
                val mapped = result match {
                  case Left(TransactionValidationError(_: AlreadyInTheState, tx)) => Right(tx -> false)
                  case Left(e)                                                    => Left(ApiError.fromValidationError(e))
                  case Right(x)                                                   => Right(x)
                }
                (mapped, log)
            }
            .toList
        }

      r.foreach { xs =>
        val newTxs = xs.collect { case (Right((tx, true)), _) => tx }
        allChannels.broadcastTx(newTxs)
      }

      r.map { xs =>
        xs.map {
          case (l @ Left(e),    trace) => TracedResult(Left(e), trace).json
          case (Right((tx, _)), trace) => TracedResult(Right(tx), trace).json
        }
      }
    }
  }

  def transfer: Route = (path("transfer") & post) {
    json[SignedTransferRequests] { transferReq =>
      doBroadcast(
        transferReq.eliminate(
          _.toTx,
          _.eliminate(
            _.toTx,
            _ => Left(UnsupportedTransactionType)
          )
        )
      )
    }
  }

  def exchange: Route = (path("exchange") & post) {
    json[SignedExchangeRequest] { req =>
      doBroadcast(req.toTx)
    }
  }
}
