package com.acrylplatform.state

import java.io.File
import java.nio.file.Files

import com.acrylplatform.account.KeyPair
import com.acrylplatform.block.Block
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.database.LevelDBWriter
import com.acrylplatform.db.LevelDBFactory
import com.acrylplatform.lagonaki.mocks.TestBlock
import com.acrylplatform.mining.MiningConstraint
import com.acrylplatform.settings.{DBSettings, FunctionalitySettings}
import com.acrylplatform.state.diffs.BlockDiffer
import com.acrylplatform.transaction.{GenesisTransaction, Transaction}
import monix.execution.UncaughtExceptionReporter
import monix.reactive.Observer
import org.iq80.leveldb.{DB, Options}
import org.openjdk.jmh.annotations.{Setup, TearDown}
import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.duration._

trait BaseState {
  import BaseState._

  private val fsSettings: FunctionalitySettings = updateFunctionalitySettings(FunctionalitySettings.TESTNET)
  private val db: DB = {
    val dir     = Files.createTempDirectory("state-synthetic").toAbsolutePath.toString
    val options = new Options()
    options.createIfMissing(true)
    LevelDBFactory.factory.open(new File(dir), options)
  }

  private val portfolioChanges = Observer.empty(UncaughtExceptionReporter.default)
  val state: LevelDBWriter = new LevelDBWriter(
    db,
    portfolioChanges,
    fsSettings,
    DBSettings("", storeTransactionsByAddress = false, storeInvokeScriptResults = false, 100000, 2000, (120 * 60 * 1000).millis))

  private var _richAccount: KeyPair = _
  def richAccount: KeyPair          = _richAccount

  private var _lastBlock: Block = _
  def lastBlock: Block          = _lastBlock

  protected def acryl(n: Float): Long = (n * 100000000L).toLong
  protected val accountGen: Gen[KeyPair] =
    Gen.containerOfN[Array, Byte](32, Arbitrary.arbitrary[Byte]).map(seed => KeyPair(seed))

  protected def updateFunctionalitySettings(base: FunctionalitySettings): FunctionalitySettings = base

  protected def txGenP(sender: KeyPair, ts: Long): Gen[Transaction]

  private def genBlock(base: Block, sender: KeyPair): Gen[Block] =
    for {
      transferTxs <- Gen.sequence[Vector[Transaction], Transaction]((1 to TxsInBlock).map { i =>
        txGenP(sender, base.timestamp + i)
      })
    } yield
      TestBlock.create(
        time = transferTxs.last.timestamp,
        ref = base.uniqueId,
        txs = transferTxs
      )

  private val initGen: Gen[(KeyPair, Block)] = for {
    rich <- accountGen
  } yield {
    val genesisTx = GenesisTransaction.create(rich, acryl(100000000L), System.currentTimeMillis() - 10000).explicitGet()
    (rich, TestBlock.create(time = genesisTx.timestamp, Seq(genesisTx)))
  }

  protected def nextBlock(txs: Seq[Transaction]): Block = TestBlock.create(
    time = txs.last.timestamp,
    ref = lastBlock.uniqueId,
    txs = txs
  )

  private def append(prev: Option[Block], next: Block): Unit = {
    val preconditionDiff = BlockDiffer.fromBlock(state, prev, next, MiningConstraint.Unlimited).explicitGet().diff
    state.append(preconditionDiff, 0, 0, None, next)
  }

  def applyBlock(b: Block): Unit = {
    append(Some(lastBlock), b)
    _lastBlock = b
  }

  def genAndApplyNextBlock(): Unit = {
    val b = genBlock(lastBlock, richAccount).sample.get
    applyBlock(b)
  }

  @Setup
  def init(): Unit = {
    val (richAccount, genesisBlock) = initGen.sample.get
    _richAccount = richAccount

    append(None, genesisBlock)
    _lastBlock = genesisBlock
  }

  @TearDown
  def close(): Unit = {
    db.close()
  }
}

object BaseState {
  private val TxsInBlock = 5000
}
