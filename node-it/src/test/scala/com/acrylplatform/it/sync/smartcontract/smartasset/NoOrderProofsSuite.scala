package com.acrylplatform.it.sync.smartcontract.smartasset

import com.acrylplatform.account.AddressScheme
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.sync.{someAssetAmount, _}
import com.acrylplatform.it.transactions.BaseTransactionSuite
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.Proofs
import com.acrylplatform.transaction.assets.BurnTransactionV2
import com.acrylplatform.transaction.smart.script.ScriptCompiler
import com.acrylplatform.transaction.transfer.TransferTransactionV2

import scala.concurrent.duration._

class NoOrderProofsSuite extends BaseTransactionSuite {
  test("try to use Order in asset scripts") {
    try {
      sender.issue(
        firstAddress,
        "assetWProofs",
        "Test coin for assetWProofs test",
        someAssetAmount,
        0,
        reissuable = true,
        issueFee,
        2,
        script = Some(
          ScriptCompiler(
            s"""
              |match tx {
              |  case o: Order => true
              |  case _ => false
              |}""".stripMargin,
            isAssetScript = true
          ).explicitGet()._1.bytes.value.base64)
      )

      fail("ScriptCompiler didn't throw expected error")
    } catch {
      case ex: java.lang.Exception => ex.getMessage should include("Compilation failed: Matching not exhaustive")
      case _: Throwable            => fail("ScriptCompiler works incorrect for orders with smart assets")
    }
  }

  test("try to use proofs in assets script") {
    val errProofMsg = "Reason: Proof doesn't validate as signature"
    val assetWProofs = sender
      .issue(
        firstAddress,
        "assetWProofs",
        "Test coin for assetWProofs test",
        someAssetAmount,
        0,
        reissuable = true,
        issueFee,
        2,
        script = Some(
          ScriptCompiler(
            s"""
                let proof = base58'assetWProofs'
                match tx {
                  case tx: SetAssetScriptTransaction | TransferTransaction | ReissueTransaction | BurnTransaction => tx.proofs[0] == proof
                  case _ => false
                }""".stripMargin,
            false
          ).explicitGet()._1.bytes.value.base64),
        waitForTx = true
      )
      .id

    val incorrectTrTx = TransferTransactionV2
      .create(
        IssuedAsset(ByteStr.decodeBase58(assetWProofs).get),
        pkByAddress(firstAddress),
        pkByAddress(thirdAddress),
        1,
        System.currentTimeMillis + 10.minutes.toMillis,
        Acryl,
        smartMinFee,
        Array.emptyByteArray,
        Proofs(Seq(ByteStr("assetWProofs".getBytes("UTF-8"))))
      )
      .right
      .get

    assertBadRequestAndMessage(
      sender.signedBroadcast(incorrectTrTx.json()),
      errProofMsg
    )

    val incorrectBrTx = BurnTransactionV2
      .create(
        AddressScheme.current.chainId,
        pkByAddress(firstAddress),
        IssuedAsset(ByteStr.decodeBase58(assetWProofs).get),
        1,
        smartMinFee,
        System.currentTimeMillis + 10.minutes.toMillis,
        Proofs(Seq(ByteStr("assetWProofs".getBytes("UTF-8"))))
      )
      .right
      .get

    assertBadRequestAndMessage(
      sender.signedBroadcast(incorrectBrTx.json()),
      errProofMsg
    )
  }

}
