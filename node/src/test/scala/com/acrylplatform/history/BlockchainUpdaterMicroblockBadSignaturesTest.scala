package com.acrylplatform.history

import com.acrylplatform.TransactionGen
import com.acrylplatform.account.KeyPair
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.crypto._
import com.acrylplatform.features.BlockchainFeatures
import com.acrylplatform.lagonaki.mocks.TestBlock
import com.acrylplatform.state.diffs._
import com.acrylplatform.transaction.GenesisTransaction
import com.acrylplatform.transaction.transfer._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class BlockchainUpdaterMicroblockBadSignaturesTest
    extends PropSpec
    with PropertyChecks
    with DomainScenarioDrivenPropertyCheck
    with Matchers
    with TransactionGen {

  val preconditionsAndPayments: Gen[(GenesisTransaction, TransferTransactionV1, TransferTransactionV1)] = for {
    master    <- accountGen
    recipient <- accountGen
    ts        <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    payment: TransferTransactionV1  <- acrylTransferGeneratorP(master, recipient)
    payment2: TransferTransactionV1 <- acrylTransferGeneratorP(master, recipient)
  } yield (genesis, payment, payment2)

  property("bad total resulting block signature") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments) {
      case (domain, (genesis, payment, payment2)) =>
        val block0                 = buildBlockOfTxs(randomSig, Seq(genesis))
        val (block1, microblocks1) = chainBaseAndMicro(block0.uniqueId, payment, Seq(payment2).map(Seq(_)))
        val badSigMicro            = microblocks1.head.copy(totalResBlockSig = randomSig)
        domain.blockchainUpdater.processBlock(block0).explicitGet()
        domain.blockchainUpdater.processBlock(block1).explicitGet()
        domain.blockchainUpdater.processMicroBlock(badSigMicro) should produce("InvalidSignature")
    }
  }

  property("bad microBlock signature") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments) {
      case (domain, (genesis, payment, payment2)) =>
        val block0                 = buildBlockOfTxs(randomSig, Seq(genesis))
        val (block1, microblocks1) = chainBaseAndMicro(block0.uniqueId, payment, Seq(payment2).map(Seq(_)))
        val badSigMicro            = microblocks1.head.copy(signature = randomSig)
        domain.blockchainUpdater.processBlock(block0).explicitGet()
        domain.blockchainUpdater.processBlock(block1).explicitGet()
        domain.blockchainUpdater.processMicroBlock(badSigMicro) should produce("InvalidSignature")
    }
  }

  property("other sender") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments) {
      case (domain, (genesis, payment, payment2)) =>
        val otherSigner = KeyPair(TestBlock.randomOfLength(KeyLength).arr)
        val block0      = buildBlockOfTxs(randomSig, Seq(genesis))
        val block1      = buildBlockOfTxs(block0.uniqueId, Seq(payment))
        val badSigMicro = buildMicroBlockOfTxs(block0.uniqueId, block1, Seq(payment2), otherSigner)._2
        domain.blockchainUpdater.processBlock(block0).explicitGet()
        domain.blockchainUpdater.processBlock(block1).explicitGet()
        domain.blockchainUpdater.processMicroBlock(badSigMicro) should produce("another account")
    }
  }
}
