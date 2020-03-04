package com.acrylplatform.it.sync.smartcontract

import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.sync.{minFee, smartMinFee}
import com.acrylplatform.it.transactions.BaseTransactionSuite
import com.acrylplatform.it.util._
import com.acrylplatform.transaction.Asset.IssuedAsset
import com.acrylplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.acrylplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class InvokeScriptWithSponsorshipSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val dApp   = pkByAddress(firstAddress)
  private val caller = pkByAddress(secondAddress)

  val quantity: Long          = 10000
  val halfQuantity: Long      = quantity / 2
  var dAppAsset: String       = ""
  var callerAsset: String     = ""
  var smartAsset: String      = ""
  var dAppInitBalance: Long   = 0
  var callerInitBalance: Long = 0

  test("_send acryl to dApp and caller accounts") {
    sender.transfer(sender.address, dApp.address, 5.acryl, minFee, waitForTx = true).id
    sender.transfer(sender.address, caller.address, 5.acryl, minFee, waitForTx = true).id
  }

  test("_issue and transfer assets") {
    dAppAsset = sender.issue(dApp.address, "dApp", "d", quantity, 0, waitForTx = true).id
    callerAsset = sender.issue(caller.address, "caller", "c", quantity, 0, waitForTx = true).id
    val script = Some(ScriptCompiler.compile("true").explicitGet()._1.bytes.value.base64)
    smartAsset = sender.issue(dApp.address, "Smart", "s", quantity, 0, script = script, waitForTx = true).id

    sender.transfer(dApp.stringRepr, caller.stringRepr, halfQuantity, minFee, Some(dAppAsset), waitForTx = true).id
    sender.transfer(caller.stringRepr, dApp.stringRepr, halfQuantity, minFee, Some(callerAsset), waitForTx = true).id
    sender.transfer(dApp.stringRepr, caller.stringRepr, halfQuantity, smartMinFee, Some(smartAsset), waitForTx = true).id
  }

  test("_enable sponsorship") {
    sender.sponsorAsset(dApp.stringRepr, dAppAsset, 1, waitForTx = true).id
    sender.sponsorAsset(caller.stringRepr, callerAsset, 1, waitForTx = true).id
  }

  test("_set scripts to dApp and caller account") {
    val dAppScript = ScriptCompiler.compile(s"""
          |{-# STDLIB_VERSION 3 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |let dAppAsset = base58'$dAppAsset'
          |let callerAsset = base58'$callerAsset'
          |let smartAsset = base58'$smartAsset'
          |
          |@Callable(i)
          |func payCallerGetDAppAsset() = {
          |  if (isDefined(i.payment) && extract(i.payment).assetId == callerAsset) then
          |    TransferSet([
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset),
          |      ScriptTransfer(i.caller, 1, dAppAsset)
          |    ])
          |  else throw("need payment in callerAsset " + toBase58String(callerAsset))
          |}
          |
          |@Callable(i)
          |func spendMaxFee() = {
          |  if (isDefined(i.payment) && extract(i.payment).assetId == smartAsset) then
          |    TransferSet([
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset),
          |      ScriptTransfer(i.caller, 1, smartAsset)
          |    ])
          |  else throw("need payment in smartAsset " + toBase58String(smartAsset))
          |}
        """.stripMargin).explicitGet()._1
    sender.setScript(dApp.address, Some(dAppScript.bytes().base64), waitForTx = true).id

    val callerScript = ScriptCompiler.compile(s"""
          |{-# STDLIB_VERSION 3 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |@Verifier(tx)
          |func verify() = {
          |  let callerAsset = base58'$callerAsset'
          |  let smartAsset = base58'$smartAsset'
          |  match (tx) {
          |    case tx:InvokeScriptTransaction =>
          |      let pay = extract(tx.payment)
          |      pay.assetId == callerAsset || pay.assetId == smartAsset
          |    case _ => false
          |  }
          |}
        """.stripMargin).explicitGet()._1
    sender.setScript(caller.stringRepr, Some(callerScript.bytes().base64), waitForTx = true).id

    val dAppScriptInfo = sender.addressScriptInfo(dApp.address)
    dAppScriptInfo.script.isEmpty shouldBe false
    dAppScriptInfo.scriptText.isEmpty shouldBe false
    dAppScriptInfo.script.get.startsWith("base64:") shouldBe true

    val smartCallerScriptInfo = sender.addressScriptInfo(caller.address)
    smartCallerScriptInfo.script.isEmpty shouldBe false
    smartCallerScriptInfo.scriptText.isEmpty shouldBe false
    smartCallerScriptInfo.script.get.startsWith("base64:") shouldBe true
  }

  test("required fee in sponsored assets considers scripts count") {
    dAppInitBalance = sender.accountBalances(dApp.address)._1
    callerInitBalance = sender.accountBalances(caller.address)._1

    val paymentAmount  = 1
    val feeAmount      = 9
    val smartFeeAmount = 53

    assertBadRequestAndMessage(
      sender.invokeScript(
        caller.address,
        dApp.address,
        Some("payCallerGetDAppAsset"),
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(callerAsset).get))),
        fee = feeAmount - 1,
        feeAssetId = Some(dAppAsset)
      ),
      s"does not exceed minimal value of 900000 ACRYL or $feeAmount"
    )
    assertBadRequestAndMessage(
      sender.invokeScript(
        caller.address,
        dApp.address,
        Some("spendMaxFee"),
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(smartAsset).get))),
        fee = smartFeeAmount - 1,
        feeAssetId = Some(dAppAsset)
      ),
      s"does not exceed minimal value of 5300000 ACRYL"
    )

    sender
      .invokeScript(
        caller.address,
        dApp.address,
        Some("payCallerGetDAppAsset"),
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(callerAsset).get))),
        fee = feeAmount,
        feeAssetId = Some(dAppAsset),
        waitForTx = true
      )
      .id
    sender
      .invokeScript(
        caller.address,
        dApp.address,
        Some("spendMaxFee"),
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(smartAsset).get))),
        fee = smartFeeAmount,
        feeAssetId = Some(dAppAsset),
        waitForTx = true
      )
      .id

    sender.assetBalance(dApp.address, dAppAsset).balance shouldBe halfQuantity + (feeAmount - 10) + smartFeeAmount
    sender.assetBalance(dApp.address, callerAsset).balance shouldBe halfQuantity + paymentAmount
    sender.accountBalances(dApp.address)._1 shouldBe dAppInitBalance - 0.009.acryl - 0.053.acryl

    sender.assetBalance(caller.address, dAppAsset).balance shouldBe halfQuantity + (-feeAmount + 10) - smartFeeAmount
    sender.assetBalance(caller.address, callerAsset).balance shouldBe halfQuantity - paymentAmount
    sender.accountBalances(caller.address)._1 shouldBe callerInitBalance
  }

  test("dApp caller is dApp address") {
    val paymentAmount = 1
    val feeAmount     = 9

    val dAppAssetBalance = sender.assetBalance(dApp.stringRepr, dAppAsset).balance
    val dAppAcrylBalance = sender.accountBalances(dApp.stringRepr)._1

    sender
      .invokeScript(
        dApp.stringRepr,
        dApp.stringRepr,
        Some("payCallerGetDAppAsset"),
        payment = Seq(Payment(paymentAmount, IssuedAsset(ByteStr.decodeBase58(callerAsset).get))),
        fee = feeAmount,
        feeAssetId = Some(dAppAsset),
        waitForTx = true
      )
      .id

    sender.assetBalance(dApp.stringRepr, dAppAsset).balance shouldBe dAppAssetBalance
    sender.accountBalances(dApp.stringRepr)._1 shouldBe dAppAcrylBalance - 0.009.acryl
  }

}
