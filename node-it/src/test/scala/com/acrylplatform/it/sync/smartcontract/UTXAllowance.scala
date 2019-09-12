package com.acrylplatform.it.sync.smartcontract

import com.typesafe.config.{Config, ConfigFactory}
import com.acrylplatform.account.KeyPair
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.sync._
import com.acrylplatform.it.transactions.NodesFromDocker
import com.acrylplatform.it.util._
import com.acrylplatform.it.{ReportingTestName, WaitForHeight2}
import com.acrylplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}

class UTXAllowance extends FreeSpec with Matchers with WaitForHeight2 with CancelAfterFailure with ReportingTestName with NodesFromDocker {
  import UTXAllowance._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def nodeA = nodes.head
  private def nodeB = nodes.last

  "create two nodes with scripted accounts and check UTX" in {
    val accounts = List(nodeA, nodeB).map(i => {

      val nodeAddress = i.createAddress()
      val acc         = KeyPair.fromSeed(i.seed(nodeAddress)).right.get

      i.transfer(i.address, nodeAddress, 10.acryl, 0.005.acryl, None, waitForTx = true)

      val scriptText = s"""true""".stripMargin
      val script               = ScriptCompiler(scriptText, isAssetScript = false).explicitGet()._1.bytes().base64
      i.setScript(acc.address, Some(script), setScriptFee, waitForTx = true)

      acc
    })

    assertBadRequestAndMessage(
      nodeA
        .transfer(
          accounts.head.address,
          recipient = accounts.head.address,
          assetId = None,
          amount = 1.acryl,
          fee = minFee + 0.004.acryl,
          version = 2
        ),
      "transactions from scripted accounts are denied from UTX pool"
    )

    val txBId =
      nodeB
        .transfer(
          accounts(1).address,
          recipient = accounts(1).address,
          assetId = None,
          amount = 1.01.acryl,
          fee = minFee + 0.004.acryl,
          version = 2
        )
        .id

    nodes.waitForHeightArise()
    nodeA.findTransactionInfo(txBId) shouldBe None
  }

}

object UTXAllowance {
  import com.acrylplatform.it.NodeConfigs._
  private val FirstNode = ConfigFactory.parseString(s"""
                                                         |acryl {
                                                         |  utx.allow-transactions-from-smart-accounts = false
                                                         |  miner {
                                                         |      quorum = 0
                                                         |      enable = yes
                                                         |  }
                                                         |}""".stripMargin)

  private val SecondNode = ConfigFactory.parseString(s"""
                                                          |acryl {
                                                          |  utx.allow-transactions-from-smart-accounts = true
                                                          |  miner {
                                                          |      enable = no
                                                          |  }
                                                          |}""".stripMargin)

  val Configs: Seq[Config] = Seq(
    FirstNode.withFallback(Default.head),
    SecondNode.withFallback(Default(1))
  )

}
