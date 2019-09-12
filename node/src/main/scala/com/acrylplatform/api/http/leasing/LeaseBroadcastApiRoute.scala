package com.acrylplatform.api.http.leasing

import akka.http.scaladsl.server.Route
import com.acrylplatform.api.http._
import com.acrylplatform.http.BroadcastRoute
import com.acrylplatform.settings.RestAPISettings
import com.acrylplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup

case class LeaseBroadcastApiRoute(settings: RestAPISettings, utx: UtxPool, allChannels: ChannelGroup)
    extends ApiRoute
    with BroadcastRoute
    with WithSettings {
  override val route = pathPrefix("leasing" / "broadcast") {
    signedLease ~ signedLeaseCancel
  }

  def signedLease: Route = (path("lease") & post) {
    json[SignedLeaseV1Request] { leaseReq =>
      doBroadcast(leaseReq.toTx)
    }
  }

  def signedLeaseCancel: Route = (path("cancel") & post) {
    json[SignedLeaseCancelV1Request] { leaseCancelReq =>
      doBroadcast(leaseCancelReq.toTx)
    }
  }
}
