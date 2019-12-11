package com.acrylplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.{CancelAfterFailure, FunSuite}
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.transactions.NodesFromDocker
import scala.concurrent.duration._

class FairPoSTestSuite extends FunSuite with CancelAfterFailure with NodesFromDocker {
  import FairPoSTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  test("blockchain grows with FairPoS activated") {
    nodes.waitForSameBlockHeadersAt(height = 10, conditionAwaitTime = 11.minutes)

    val txId = nodes.head.transfer(nodes.head.address, nodes.last.address, transferAmount, minFee).id
    nodes.last.waitForTransaction(txId)

    val heightAfterTransfer = nodes.head.height

    nodes.waitForSameBlockHeadersAt(heightAfterTransfer + 10, conditionAwaitTime = 11.minutes)
  }
}

object FairPoSTestSuite {
  import com.acrylplatform.it.NodeConfigs._
  private val microblockActivationHeight = 0
  private val fairPoSActivationHeight    = 10

  private val config =
    ConfigFactory.parseString(s"""
    |acryl {
    |   blockchain.custom {
    |      functionality {
    |        pre-activated-features {1 = $microblockActivationHeight, 8 = $fairPoSActivationHeight}
    |        generation-balance-depth-from-50-to-1000-after-height = 1000
    |      }
    |   }
    |   miner.quorum = 1
    |}""".stripMargin)

  val Configs: Seq[Config] = Default.map(config.withFallback(_)).take(3)
}
