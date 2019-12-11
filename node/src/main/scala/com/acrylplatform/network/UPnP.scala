package com.acrylplatform.network

import java.net.{InetAddress, InetSocketAddress}

import com.acrylplatform.settings.UPnPSettings
import com.acrylplatform.utils.ScorexLogging
import org.bitlet.weupnp.{GatewayDevice, GatewayDiscover, PortMappingEntry}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

class UPnP(settings: UPnPSettings) extends ScorexLogging {

  private var gateway: Option[GatewayDevice] = None

  lazy val localAddress: Option[InetAddress] = gateway.map(_.getLocalAddress)
  lazy val externalAddress: Option[InetAddress] = {
    val ipUPnP = gateway.map(_.getExternalIPAddress).map(InetAddress.getByName) match {
      case Some(value) => value.getHostAddress
      case None        => "Unknown"
    }

    val ipService = Try {
      scala.io.Source.fromURL("https://api.ipify.org/?format=txt")
    } match {
      case Success(value) => value.mkString
      case Failure(_)     => "Unknown"
    }

    if (ipUPnP == "Unknown" && ipService == "Unknown") None
    else if (ipService == "Unknown") Option(InetAddress.getByName(ipUPnP))
    else if (ipUPnP == "Unknown") Option(InetAddress.getByName(ipService))
    else if (ipUPnP == ipService) Option(InetAddress.getByName(ipUPnP))
    else Option(InetAddress.getByName(ipService))
  }

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

  def addPort(port: Int): Either[String, InetSocketAddress] =
    if (externalAddress.nonEmpty && localAddress.nonEmpty)
      portMapping(localAddress.get, port, port, 20) match {
        case 0            => Left("Unable to map port")
        case newPort: Int => Right(new InetSocketAddress(externalAddress.get.getHostAddress, newPort))
      } else Left("No external or local address")

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

  def portMapping(address: InetAddress, externalPort: Int, internalPort: Int, acc: Int): Int = {
    val newPort = scala.util.Random.nextInt(55535) + 10000
    if (acc == 0)
      0
    else if (gateway.get.getSpecificPortMappingEntry(externalPort, "TCP", new PortMappingEntry()))
      portMapping(address, newPort, internalPort, acc - 1)
    else if (gateway.get.addPortMapping(externalPort, internalPort, address.getHostAddress, "TCP", "Scorex"))
      externalPort
    else
      portMapping(address, newPort, internalPort, acc - 1)
  }
}
