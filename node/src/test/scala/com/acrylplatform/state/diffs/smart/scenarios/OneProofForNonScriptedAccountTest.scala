package com.acrylplatform.state.diffs.smart.scenarios

import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.lagonaki.mocks.TestBlock
import com.acrylplatform.lang.v1.compiler.Terms._
import com.acrylplatform.state.diffs.smart.smartEnabledFS
import com.acrylplatform.state.diffs.{ENOUGH_AMT, assertDiffEi, produce}
import com.acrylplatform.transaction.Asset.Acryl
import com.acrylplatform.lang.script.v1.ExprScript
import com.acrylplatform.transaction.transfer._
import com.acrylplatform.transaction.{GenesisTransaction, Proofs}
import com.acrylplatform.{NoShrink, TransactionGen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class OneProofForNonScriptedAccountTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("exactly 1 proof required for non-scripted accounts") {
    val s = for {
      master    <- accountGen
      recepient <- accountGen
      amt       <- positiveLongGen
      fee       <- smallFeeGen
      ts        <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      setScript <- selfSignedSetScriptTransactionGenP(master, ExprScript(TRUE).explicitGet())
      transfer = TransferTransactionV2.selfSigned(Acryl, master, recepient, amt, ts, Acryl, fee, Array.emptyByteArray).explicitGet()
    } yield (genesis, setScript, transfer)

    forAll(s) {
      case (genesis, script, transfer) =>
        val transferWithExtraProof = transfer.copy(proofs = Proofs(Seq(ByteStr.empty, ByteStr(Array(1: Byte)))))
        assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transferWithExtraProof)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("must have exactly 1 proof"))
    }
  }

}
