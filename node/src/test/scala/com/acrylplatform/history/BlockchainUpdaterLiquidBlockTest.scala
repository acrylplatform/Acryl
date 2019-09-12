package com.acrylplatform.history

import com.acrylplatform._
import com.acrylplatform.block.{Block, MicroBlock}
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.lagonaki.mocks.TestBlock
import com.acrylplatform.state.diffs.ENOUGH_AMT
import com.acrylplatform.transaction.GenesisTransaction
import com.acrylplatform.transaction.TxValidationError.GenericError
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class BlockchainUpdaterLiquidBlockTest
    extends PropSpec
    with PropertyChecks
    with DomainScenarioDrivenPropertyCheck
    with Matchers
    with TransactionGen
    with BlocksTransactionsHelpers {
  import QuickTX._
  import UnsafeBlocks._

  private def preconditionsAndPayments(minTx: Int, maxTx: Int): Gen[(Block, Block, Seq[MicroBlock])] =
    for {
      richAccount        <- accountGen
      totalTxNumber      <- Gen.chooseNum(minTx, maxTx)
      txNumberInKeyBlock <- Gen.chooseNum(0, Block.MaxTransactionsPerBlockVer3)
      allTxs             <- Gen.listOfN(totalTxNumber, transfer(richAccount))
    } yield {
      val (keyBlockTxs, microTxs) = allTxs.splitAt(txNumberInKeyBlock)
      val txNumberInMicros        = totalTxNumber - txNumberInKeyBlock

      val prevBlock = unsafeBlock(
        reference = randomSig,
        txs = Seq(GenesisTransaction.create(richAccount, ENOUGH_AMT, 0).explicitGet()),
        signer = TestBlock.defaultSigner,
        version = 3,
        timestamp = 0
      )

      val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
        totalRefTo = prevBlock.signerData.signature,
        base = keyBlockTxs,
        micros = microTxs.grouped(math.max(1, txNumberInMicros / 5)).toSeq,
        signer = TestBlock.defaultSigner,
        version = 3,
        timestamp = System.currentTimeMillis()
      )

      (prevBlock, keyBlock, microBlocks)
    }

  property("liquid block can't be overfilled") {
    import Block.{MaxTransactionsPerBlockVer3 => Max}
    forAll(preconditionsAndPayments(Max + 1, Max + 100)) {
      case (prevBlock, keyBlock, microBlocks) =>
        withDomain(MicroblocksActivatedAt0AcrylSettings) { d =>
          val blocksApplied = for {
            _ <- d.blockchainUpdater.processBlock(prevBlock)
            _ <- d.blockchainUpdater.processBlock(keyBlock)
          } yield ()

          val r = microBlocks.foldLeft(blocksApplied) {
            case (Right(_), curr) => d.blockchainUpdater.processMicroBlock(curr)
            case (x, _)           => x
          }

          withClue("All microblocks should not be processed") {
            r match {
              case Left(e: GenericError) => e.err should include("Limit of txs was reached")
              case x =>
                val txNumberByMicroBlock = microBlocks.map(_.transactionData.size)
                fail(
                  s"Unexpected result: $x. keyblock txs: ${keyBlock.transactionCount}, " +
                    s"microblock txs: ${txNumberByMicroBlock.mkString(", ")} (total: ${txNumberByMicroBlock.sum}), " +
                    s"total txs: ${keyBlock.transactionCount + txNumberByMicroBlock.sum}")
            }
          }
        }
    }
  }

  property("miner settings don't interfere with micro block processing") {
    val oneTxPerMicroSettings = MicroblocksActivatedAt0AcrylSettings
      .copy(minerSettings = MicroblocksActivatedAt0AcrylSettings.minerSettings.copy(
          maxTransactionsInMicroBlock = 1
        ))
    forAll(preconditionsAndPayments(10, Block.MaxTransactionsPerBlockVer3)) {
      case (genBlock, keyBlock, microBlocks) =>
        withDomain(oneTxPerMicroSettings) { d =>
          d.blockchainUpdater.processBlock(genBlock)
          d.blockchainUpdater.processBlock(keyBlock)
          microBlocks.foreach { mb =>
            d.blockchainUpdater.processMicroBlock(mb) shouldBe 'right
          }
        }
    }
  }
}
