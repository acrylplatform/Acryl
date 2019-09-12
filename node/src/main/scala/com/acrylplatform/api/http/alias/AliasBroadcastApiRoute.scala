package com.acrylplatform.api.http.alias

import akka.http.scaladsl.server.Route
import com.acrylplatform.api.http._
import com.acrylplatform.http.BroadcastRoute
import com.acrylplatform.settings.RestAPISettings
import com.acrylplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup

case class AliasBroadcastApiRoute(settings: RestAPISettings, utx: UtxPool, allChannels: ChannelGroup)
    extends ApiRoute
    with BroadcastRoute
    with WithSettings {
  override val route = pathPrefix("alias" / "broadcast") {
    signedCreate
  }

  def signedCreate: Route = (path("create") & post) {
    json[SignedCreateAliasV1Request] { aliasReq =>
      doBroadcast(aliasReq.toTx)
    }
  }
}
