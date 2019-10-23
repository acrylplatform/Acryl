package com.acrylplatform.network

import java.net.InetAddress
import com.acrylplatform.settings.UPnPSettings
import com.acrylplatform.utils.ScorexLogging
import org.bitlet.weupnp.{GatewayDevice, GatewayDiscover}
import scala.collection.JavaConverters._
import scala.util.Try

class UPnP(settings: UPnPSettings) extends ScorexLogging {

  private var gateway: Option[GatewayDevice] = None

  lazy val localAddress: Option[InetAddress]    = gateway.map(_.getLocalAddress)
  lazy val externalAddress: Option[InetAddress] = gateway.map(_.getExternalIPAddress).map(InetAddress.getByName)

  Try {
    log.info("Looking for UPnP gateway device...")
    val defaultHttpReadTimeout = settings.gatewayTimeout
    GatewayDevice.setHttpReadTimeout(defaultHttpReadTimeout.toMillis.toInt)
    val discover               = new GatewayDiscover()
    val defaultDiscoverTimeout = settings.discoverTimeout
    discover.setTimeout(defaultDiscoverTimeout.toMillis.toInt)

    val gatewayMap = Option(discover.discover).map(_.asScala.toMap).getOrElse(Map())
    if (gatewayMap.isEmpty) {
      log.debug("There are no UPnP gateway devices")
    } else {
      gatewayMap.foreach {
        case (addr, _) =>
          log.debug("UPnP gateway device found on " + addr.getHostAddress)
      }
      Option(discover.getValidGateway) match {
        case None => log.debug("There is no connected UPnP gateway device")
        case Some(device) =>
          gateway = Some(device)
          log.debug("Using UPnP gateway device on " + localAddress.map(_.getHostAddress).getOrElse("err"))
          log.info("External IP address is " + externalAddress.map(_.getHostAddress).getOrElse("err"))
      }
    }
  }.recover {
    case t: Throwable =>
      log.error("Unable to discover UPnP gateway devices: " + t.toString)
  }

  def addPort(port: Int): Either[InetAddress, String] = externalAddress match {
    case Some(externalAddr) =>
      localAddress match {
        case Some(address) =>
          if (gateway.get.addPortMapping(port, port, address.getHostAddress, "TCP", "Scorex"))
            Left(externalAddr)
          else
            Right("Unable to map port " + port)
        case None => Right("No local address")
      }
    case None => Right("No external address")
  }

  def deletePort(port: Int): Try[Unit] =
    Try {
      if (gateway.get.deletePortMapping(port, "TCP")) {
        log.debug("Mapping deleted for port " + port)
      } else {
        log.debug("Unable to delete mapping for port " + port)
      }
    }.recover {
      case t: Throwable =>
        log.error("Unable to delete mapping for port " + port + ": " + t.toString)
    }
}
