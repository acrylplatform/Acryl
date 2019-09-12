package com.acrylplatform.it.sync.smartcontract

import com.acrylplatform.transaction.Asset.Acryl
import com.acrylplatform.transaction.smart.SetScriptTransaction
import com.acrylplatform.transaction.smart.script.ScriptCompiler
import com.acrylplatform.transaction.transfer.TransferTransactionV2
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.sync._
import com.acrylplatform.it.transactions.BaseTransactionSuite
import com.acrylplatform.it.util._
import org.scalatest.CancelAfterFailure
import com.acrylplatform.common.utils.EitherExt2

class BigLetChain extends BaseTransactionSuite with CancelAfterFailure {
  test("big let assignment chain") {
    val count = 550
    val scriptText =
      s"""
         | {-# STDLIB_VERSION 3    #-}
         | {-# CONTENT_TYPE   DAPP #-}
         |
         | @Verifier(tx)
         | func verify() = {
         |   let a0 = 1
         |   ${1 to count map (i => s"let a$i = a${i - 1}") mkString "\n"}
         |   a$count == a$count
         | }
       """.stripMargin

    val compiledScript = ScriptCompiler.compile(scriptText).explicitGet()._1

    val newAddress   = sender.createAddress()
    val acc0         = pkByAddress(firstAddress)
    val pkNewAddress = pkByAddress(newAddress)

    sender.transfer(acc0.address, newAddress, 10.acryl, minFee, waitForTx = true)

    val scriptSet = SetScriptTransaction.selfSigned(
      pkNewAddress,
      Some(compiledScript),
      setScriptFee,
      System.currentTimeMillis()
    )
    val scriptSetBroadcast = sender.signedBroadcast(scriptSet.explicitGet().json.value)
    nodes.waitForHeightAriseAndTxPresent(scriptSetBroadcast.id)

    val transfer = TransferTransactionV2.selfSigned(
      Acryl,
      pkNewAddress,
      pkNewAddress,
      1.acryl,
      System.currentTimeMillis(),
      Acryl,
      smartMinFee,
      Array()
    )
    val transferBroadcast = sender.signedBroadcast(transfer.explicitGet().json.value)
    nodes.waitForHeightAriseAndTxPresent(transferBroadcast.id)
  }
}
