package com.acrylplatform.state.diffs

import java.util.concurrent.ThreadLocalRandom

import com.acrylplatform.BlockGen
import com.acrylplatform.account.KeyPair
import com.acrylplatform.block.Block
import com.acrylplatform.common.utils._
import com.acrylplatform.crypto.KeyLength
import com.acrylplatform.db.WithDomain
import com.acrylplatform.features.BlockchainFeatures
import com.acrylplatform.lagonaki.mocks.TestBlock
import com.acrylplatform.lang.ValidationError
import com.acrylplatform.mining.MiningConstraint
import com.acrylplatform.settings.FunctionalitySettings
import com.acrylplatform.state.Blockchain
import com.acrylplatform.state.reader.CompositeBlockchain
import com.acrylplatform.transaction.GenesisTransaction
import org.scalatest.{Assertion, FreeSpecLike, Matchers}

import scala.concurrent.duration._

class BlockDiffWithRewardTest extends FreeSpecLike with Matchers with BlockGen with WithDomain {

  /*
      | N | fee | reward | signer | A receive | A balance | B receive | B balance |
      |--:|:---:|:------:|:------:|----------:|----------:|----------:|-----------|
      |1  |0    |0       |A       |0          |0          |0          |0          | <- genesis
      |2  |10   |0       |B       |0          |0          |10         |+10        |
      |3  |10   |0       |A       |10         |+10        |0          |0          |
      |4  |10   |0       |B       |0          |10         |+10        |10+10=20   |
      |---------------------- Enable NG and BlockReward --------------------------|
      |5  |10   |6       |A       |4+6=10     |10+10=20   |0          |20         |
      |6  |10   |6       |B       |0          |20         |+4+6+6=10  |20+16=36   |
      |7  |10   |6       |A       |4+6+6=16   |20+16=36   |0          |36         |
      |8  |10   |6       |B       |0          |36         |+4+6+6=16  |36+16=52   |
      |9  |10   |6       |A       |4+6+6=16   |36+16=52   |0          |52         | <- 1st check
      |10 |10   |6       |B       |0          |52         |+4+6+6=16  |52+16=68   | <- 2nd check
   */
  "height > BlockReward activation - a miner should receive 60% of previous block's fee and 40% of the current one + reward" in {
    assertBlockchain(testChain.init) { s =>
      s.balance(signerA) shouldBe 52
    }

    assertBlockchain(testChain) { s =>
      s.balance(signerB) shouldBe 68
    }
  }

  private def assertBlockchain(chain: Seq[Block])(test: Blockchain => Assertion) = {
    val fs = FunctionalitySettings(
      featureCheckBlocksPeriod = 2,
      blocksForFeatureActivation = 1,
      preActivatedFeatures = Map(BlockchainFeatures.NG.id -> 4, BlockchainFeatures.BlockReward.id -> 4),
      doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
      allowTemporaryNegativeUntil = 0,
      generationBalanceDepthFrom50To1000AfterHeight = 0,
      minimalGeneratingBalanceAfter = 0,
      allowTransactionsFromFutureUntil = 0,
      allowUnissuedAssetsUntil = 0,
      allowInvalidReissueInSameBlockUntilTimestamp = 0,
      allowMultipleLeaseCancelTransactionUntilTimestamp = 0,
      resetEffectiveBalancesAtHeight = 1,
      blockVersion3AfterHeight = 0,
      maxTransactionTimeBackOffset = 120.minutes,
      maxTransactionTimeForwardOffset = 90.minutes
    )
    withLevelDBWriter(fs) { state =>
      def differ(blockchain: Blockchain, prevBlock: Option[Block], b: Block): Either[ValidationError, BlockDiffer.GenResult] =
        BlockDiffer.fromBlock(CompositeBlockchain(blockchain, reward = Some(6)), prevBlock, b, MiningConstraint.Unlimited)

      chain.init.foldLeft[Option[Block]](None) { (prevBlock, curBlock) =>
        val BlockDiffer.Result(diff, fees, totalFee, _) = differ(state, prevBlock, curBlock).explicitGet()
        state.append(diff, fees, totalFee, None, curBlock)
        Some(curBlock)
      }

      val BlockDiffer.Result(diff, fees, totalFee, _) = differ(state, chain.init.lastOption, chain.last).explicitGet()
      val cb                                          = CompositeBlockchain(state, Some(diff))
      test(cb)

      state.append(diff, fees, totalFee, None, chain.last)
      test(state)
    }
  }

  private def randomKeyPair(): KeyPair = {
    val seed = Array.ofDim[Byte](KeyLength)
    ThreadLocalRandom.current().nextBytes(seed)
    KeyPair(seed)
  }

  private val signerA, signerB = randomKeyPair()

  private val TransactionFee = 10

  val testChain: Seq[Block] = {
    val from, to             = randomKeyPair()
    val ts                   = System.currentTimeMillis() - 100000
    val genesisTx            = GenesisTransaction.create(from, Long.MaxValue - 1, ts).explicitGet()
    val features: Set[Short] = Set[Short](BlockchainFeatures.NG.id, BlockchainFeatures.BlockReward.id)

    val paymentTxs = (1 to 9).map { i =>
      createAcrylTransfer(
        from,
        to,
        amount = 10000,
        TransactionFee,
        timestamp = ts + i * 1000
      ).explicitGet()
    }

    (genesisTx +: paymentTxs).zipWithIndex.map {
      case (x, i) =>
        val signer = if (i % 2 == 0) signerA else signerB
        TestBlock.create(signer, Seq(x), features)
    }
  }
}
