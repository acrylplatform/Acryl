package com.acrylplatform.it.sync.debug

import com.typesafe.config.Config
import com.acrylplatform.it.{Node, NodeConfigs}
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.transactions.NodesFromDocker
import com.acrylplatform.it.util._
import com.acrylplatform.it.sync._
import org.scalatest.FunSuite

class DebugPortfoliosSuite extends FunSuite with NodesFromDocker {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(entitiesNumber = 1)
      .buildNonConflicting()

  private def sender: Node = nodes.head

  private val firstAddress  = sender.createAddress()
  private val secondAddress = sender.createAddress()

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    sender.transfer(sender.address, firstAddress, 20.acryl, minFee, waitForTx = true)
    sender.transfer(sender.address, secondAddress, 20.acryl, minFee, waitForTx = true)
  }

  test("getting a balance considering pessimistic transactions from UTX pool - changed after UTX") {
    val portfolioBefore = sender.debugPortfoliosFor(firstAddress, considerUnspent = true)
    val utxSizeBefore   = sender.utxSize

    sender.transfer(firstAddress, secondAddress, 5.acryl, 5.acryl)
    sender.transfer(secondAddress, firstAddress, 7.acryl, 5.acryl)

    sender.waitForUtxIncreased(utxSizeBefore)

    val portfolioAfter = sender.debugPortfoliosFor(firstAddress, considerUnspent = true)

    val expectedBalance = portfolioBefore.balance - 10.acryl // withdraw + fee
    assert(portfolioAfter.balance == expectedBalance)

  }

  test("getting a balance without pessimistic transactions from UTX pool - not changed after UTX") {
    nodes.waitForHeightArise()

    val portfolioBefore = sender.debugPortfoliosFor(firstAddress, considerUnspent = false)
    val utxSizeBefore   = sender.utxSize

    sender.transfer(firstAddress, secondAddress, 5.acryl, fee = 5.acryl)
    sender.waitForUtxIncreased(utxSizeBefore)

    val portfolioAfter = sender.debugPortfoliosFor(firstAddress, considerUnspent = false)
    assert(portfolioAfter.balance == portfolioBefore.balance)
  }
}
