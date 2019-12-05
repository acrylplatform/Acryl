package com.acrylplatform.utils

import java.io.File
import java.lang.management.ManagementFactory.getRuntimeMXBean
import java.net.InetAddress

import com.acrylplatform.account.Address
import com.acrylplatform.network.NS
import com.acrylplatform.state.Blockchain
import com.acrylplatform.wallet.Wallet
import com.acrylplatform.Version
import monix.eval.Task
import monix.execution.{Cancelable, CancelableFuture, Scheduler}
import monix.execution.schedulers.SchedulerService

import scala.concurrent.duration._
import scala.io.Source
import scala.util.{Failure, Success, Try}

object NodeStatus extends ScorexLogging {
  private implicit val scheduler: SchedulerService = Scheduler.singleThread("node-status")

  def start(enable: Boolean, blockchain: Blockchain, wallet: Wallet, network: NS, localAddress: Option[InetAddress]): CancelableFuture[Cancelable] =
    Task {
      // TODO: Disable logger
      val scheduler = Scheduler.io("NodeStatus")
      scheduler.scheduleAtFixedRate(0.seconds, 120.seconds) {
        val pid = getRuntimeMXBean.getName.split("@")(0)

        def networkConnectionStatus: String =
          Try {
            List("https://ya.ru/", "https://yahoo.com/")
              .map(url => Source.fromURL(url).mkString)
              .filter(data => !data.isEmpty)
          } match {
            case Success(value) => if (value.isEmpty) "Failed" else "OK"
            case Failure(_)     => "Failed"
          }

        def syncStatus: String = {
          val heightLocal  = blockchain.height
          val heightRemote = 1000000 // TODO: Get remote node

          if (heightRemote != heightLocal)
            s"($heightLocal/$heightRemote)"
          else
            s"($heightLocal)"
        }

        def checkBalance(address: Address): Double =
          blockchain.balance(address) / 100000000

        val (nodeAddress, balance, effectiveBalance, effectiveBalanceNoConfirmations) =
          if (wallet.privateKeyAccounts.isEmpty)
            ("Unknown", "Unknown", "Unknown", "Unknown")
          else {
            val addr                      = wallet.privateKeyAccounts.head.toAddress
            val balance                   = checkBalance(addr)
            val effBalance                = blockchain.effectiveBalance(addr, 1000)
            val effBalanceNoConfirmations = blockchain.effectiveBalance(addr, 0)
            (addr, balance, effBalance, effBalanceNoConfirmations)
          }

        val disk = Try {
          val file  = new File("/")
          val total = (file.getTotalSpace / 1000000000).floor + "GB"
          val used  = ((file.getTotalSpace - file.getUsableSpace) / 1000000000).floor + "GB"
          val free  = (file.getUsableSpace / 1000000000).floor + "GB"
          total + "/" + used + "/" + free
        } match {
          case Success(value) => value
          case Failure(_)     => "Unknown"
        }

        val internalIP = localAddress match {
          case Some(value) => value.getHostAddress
          case None        => "Unknown"
        }

        val remoteVersion = "Unknown"

        val version = Version.VersionString + " " + s"($remoteVersion)"

        val currentTimestamp = java.time.LocalDate.now.toString + " " + java.time.LocalTime.now.toString

        //noinspection ScalaStyle
        println(s"""
                   |Node status: Running (pid $pid)
                   |Initialization: OK
                   |Network connection: $networkConnectionStatus
                   |Synchronized: $syncStatus
                   |Address: $nodeAddress
                   |Balance: $balance
                   |Effective balance (1000 confirmations): $effectiveBalance
                   |Effective balance (0 confirmations): $effectiveBalanceNoConfirmations
                   |Disk (total/used/free): $disk
                   |Internal IP: $internalIP
                   |Node version: $version
                   |Message generated on $currentTimestamp
                   |""".stripMargin)
      }
    }.runAsyncLogErr
}
