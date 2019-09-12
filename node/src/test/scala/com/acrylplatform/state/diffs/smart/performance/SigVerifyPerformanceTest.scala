package com.acrylplatform.state.diffs.smart.performance

import com.acrylplatform.account.{KeyPair, PublicKey}
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.lagonaki.mocks.TestBlock
import com.acrylplatform.lang.directives.values._
import com.acrylplatform.lang.v1.compiler.ExpressionCompiler
import com.acrylplatform.lang.v1.compiler.Terms._
import com.acrylplatform.lang.v1.parser.Parser
import com.acrylplatform.metrics.Instrumented
import com.acrylplatform.state.diffs._
import com.acrylplatform.state.diffs.smart._
import com.acrylplatform.transaction.Asset.Acryl
import com.acrylplatform.transaction.GenesisTransaction
import com.acrylplatform.lang.script.v1.ExprScript
import com.acrylplatform.transaction.transfer._
import com.acrylplatform.lang.utils._
import com.acrylplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class SigVerifyPerformanceTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  private val AmtOfTxs = 10000

  private def simpleSendGen(from: KeyPair, to: PublicKey, ts: Long): Gen[TransferTransactionV1] =
    for {
      amt <- smallFeeGen
      fee <- smallFeeGen
    } yield TransferTransactionV1.selfSigned(Acryl, from, to.toAddress, amt, ts, Acryl, fee, Array.emptyByteArray).explicitGet()

  private def scriptedSendGen(from: KeyPair, to: PublicKey, ts: Long): Gen[TransferTransactionV2] =
    for {
      amt <- smallFeeGen
      fee <- smallFeeGen
    } yield TransferTransactionV2.selfSigned(Acryl, from, to.toAddress, amt, ts, Acryl, fee, Array.emptyByteArray).explicitGet()

  private def differentTransfers(typed: EXPR) =
    for {
      master    <- accountGen
      recipient <- accountGen
      ts        <- positiveIntGen
      amt       <- smallFeeGen
      fee       <- smallFeeGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      setScript <- selfSignedSetScriptTransactionGenP(master, ExprScript(typed).explicitGet())
      transfer       = simpleSendGen(master, recipient, ts)
      scriptTransfer = scriptedSendGen(master, recipient, ts)
      transfers       <- Gen.listOfN(AmtOfTxs, transfer)
      scriptTransfers <- Gen.listOfN(AmtOfTxs, scriptTransfer)
    } yield (genesis, setScript, transfers, scriptTransfers)

  ignore("parallel native signature verification vs sequential scripted signature verification") {
    val textScript    = "sigVerify(tx.bodyBytes,tx.proofs[0],tx.senderPk)"
    val untypedScript = Parser.parseExpr(textScript).get.value
    val typedScript   = ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), untypedScript).explicitGet()._1

    forAll(differentTransfers(typedScript)) {
      case (gen, setScript, transfers, scriptTransfers) =>
        def simpleCheck(): Unit = assertDiffAndState(Seq(TestBlock.create(Seq(gen))), TestBlock.create(transfers), smartEnabledFS) { case _ => }
        def scriptedCheck(): Unit =
          assertDiffAndState(Seq(TestBlock.create(Seq(gen, setScript))), TestBlock.create(scriptTransfers), smartEnabledFS) {
            case _ =>
          }

        val simeplCheckTime   = Instrumented.withTimeMillis(simpleCheck())._2
        val scriptedCheckTime = Instrumented.withTimeMillis(scriptedCheck())._2
        println(s"[parallel] simple check time: $simeplCheckTime ms,\t [seqential] scripted check time: $scriptedCheckTime ms")
    }

  }
}
