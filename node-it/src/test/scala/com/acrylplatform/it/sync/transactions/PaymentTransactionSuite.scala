package com.acrylplatform.it.sync.transactions

import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.api.PaymentRequest
import com.acrylplatform.it.transactions.BaseTransactionSuite
import com.acrylplatform.it.util._
import org.scalatest.prop.TableDrivenPropertyChecks

class PaymentTransactionSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {

  private val paymentAmount = 5.acryl
  private val defaulFee     = 1.acryl

  test("acryl payment changes acryl balances and eff.b.") {

    val (firstBalance, firstEffBalance)   = miner.accountBalances(firstAddress)
    val (secondBalance, secondEffBalance) = miner.accountBalances(secondAddress)

    val transferId = sender.payment(firstAddress, secondAddress, paymentAmount, defaulFee).id
    nodes.waitForHeightAriseAndTxPresent(transferId)
    miner.assertBalances(firstAddress, firstBalance - paymentAmount - defaulFee, firstEffBalance - paymentAmount - defaulFee)
    miner.assertBalances(secondAddress, secondBalance + paymentAmount, secondEffBalance + paymentAmount)
  }

  val payment = PaymentRequest(5.acryl, 1.acryl, firstAddress, secondAddress)
  val endpoints =
    Table("/acryl/payment/signature", "/acryl/create-signed-payment", "/acryl/external-payment", "/acryl/broadcast-signed-payment")
  forAll(endpoints) { (endpoint: String) =>
    test(s"obsolete endpoints respond with BadRequest. Endpoint:$endpoint") {
      val errorMessage = "This API is no longer supported"
      assertBadRequestAndMessage(sender.postJson(endpoint, payment), errorMessage)
    }
  }
}
