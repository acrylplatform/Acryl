package com.acrylplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.acrylplatform.account.AddressOrAlias
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.transactions.NodesFromDocker
import com.acrylplatform.it.util._
import com.acrylplatform.it.{ReportingTestName, WaitForHeight2}
import com.acrylplatform.transaction.Asset.Acryl
import com.acrylplatform.transaction.transfer.TransferTransactionV2
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}

class NodeRestartTestSuite extends FreeSpec with Matchers with WaitForHeight2 with CancelAfterFailure with ReportingTestName with NodesFromDocker {
  import NodeRestartTestSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def nodeA = nodes.head
  private def nodeB = nodes(1)

  "node should grow up to 5 blocks together and sync" in {
    nodes.waitForSameBlockHeadersAt(height = 5)
  }

  "create many addresses and check them after node restart" in {
    1 to 10 map (_ => nodeA.createAddress())
    val setOfAddresses      = nodeA.getAddresses
    val nodeAWithOtherPorts = docker.restartContainer(dockerNodes().head)
    val maxHeight           = nodes.map(_.height).max
    nodeAWithOtherPorts.getAddresses should contain theSameElementsAs setOfAddresses
    nodes.waitForSameBlockHeadersAt(maxHeight + 2)
  }

  "after restarting all the nodes, the duplicate transaction cannot be put into the blockchain" in {
    val txJson = TransferTransactionV2
      .selfSigned(Acryl,
                  nodeB.privateKey,
                  AddressOrAlias.fromString(nodeA.address).explicitGet(),
                  1.acryl,
                  System.currentTimeMillis(),
                  Acryl,
                  minFee,
                  Array[Byte]())
      .explicitGet()
      .json()

    val tx = nodeB.signedBroadcast(txJson, waitForTx = true)
    nodeA.waitForTransaction(tx.id)

    val txHeight = nodeA.transactionInfo(tx.id).height

    nodes.waitForHeightArise()

    docker.restartContainer(dockerNodes().head)
    docker.restartContainer(dockerNodes()(1))

    nodes.waitForHeight(txHeight + 2)

    assertBadRequestAndMessage(nodeB.signedBroadcast(txJson, waitForTx = true),
                               s"State check failed. Reason: Transaction ${tx.id} is already in the state on a height of $txHeight")
  }

}

object NodeRestartTestSuite {
  import com.acrylplatform.it.NodeConfigs._
  private val FirstNode = ConfigFactory.parseString(s"""
                                                         |acryl {
                                                         |  synchronization.synchronization-timeout = 10s
                                                         |  blockchain.custom.functionality {
                                                         |    pre-activated-features.1 = 0
                                                         |  }
                                                         |  miner.quorum = 0
                                                         |  wallet {
                                                         |     file = "/tmp/wallet.dat"
                                                         |     password = "bla"
                                                         |  }
                                                         |
                                                         |}""".stripMargin)

  private val SecondNode = ConfigFactory.parseString(s"""
                                                            |acryl {
                                                            |  synchronization.synchronization-timeout = 10s
                                                            |  blockchain.custom.functionality {
                                                            |    pre-activated-features.1 = 0
                                                            |  }
                                                            |  miner.enable = no
                                                            |  wallet {
                                                            |     file = "/tmp/wallet.dat"
                                                            |     password = "bla"
                                                            |  }
                                                            |}""".stripMargin)

  val Configs: Seq[Config] = Seq(
    FirstNode.withFallback(Default.head),
    SecondNode.withFallback(Default(1))
  )

}
