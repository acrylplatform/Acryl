package com.acrylplatform.api.http.leasing

import akka.http.scaladsl.server.Route
import com.acrylplatform.account.Address
import com.acrylplatform.api.common.CommonAccountApi
import com.acrylplatform.api.http._
import com.acrylplatform.http.BroadcastRoute
import com.acrylplatform.settings.RestAPISettings
import com.acrylplatform.state.Blockchain
import com.acrylplatform.transaction._
import com.acrylplatform.transaction.lease.LeaseTransaction
import com.acrylplatform.utils.Time
import com.acrylplatform.utx.UtxPool
import com.acrylplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.JsNumber

@Path("/leasing")
@Api(value = "/leasing")
case class LeaseApiRoute(settings: RestAPISettings, wallet: Wallet, blockchain: Blockchain, utx: UtxPool, allChannels: ChannelGroup, time: Time)
    extends ApiRoute
    with BroadcastRoute
    with WithSettings {

  private[this] val commonAccountApi = new CommonAccountApi(blockchain)

  override val route: Route = pathPrefix("leasing") {
    lease ~ cancel ~ active
  }

  def lease: Route = processRequest("lease", (t: LeaseV1Request) => doBroadcast(TransactionFactory.leaseV1(t, wallet, time)))

  def cancel: Route = processRequest("cancel", (t: LeaseCancelV1Request) => doBroadcast(TransactionFactory.leaseCancelV1(t, wallet, time)))

  @Path("/active/{address}")
  @ApiOperation(value = "Get all active leases for an address", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Wallet address ", required = true, dataType = "string", paramType = "path")
    ))
  def active: Route = (pathPrefix("active") & get & extractScheduler) { implicit sc =>
    pathPrefix(Segment) { address =>
      complete(Address.fromString(address) match {
        case Left(e) => ApiError.fromValidationError(e)
        case Right(a) =>
          commonAccountApi
            .activeLeases(a)
            .collect {
              case (height, leaseTransaction: LeaseTransaction) =>
                leaseTransaction.json() + ("height" -> JsNumber(height))
            }
            .toListL
            .runToFuture
      })
    }
  }
}
