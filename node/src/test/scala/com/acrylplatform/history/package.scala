package com.acrylplatform

import com.typesafe.config.{Config, ConfigFactory}
import com.acrylplatform.account.KeyPair
import com.acrylplatform.block.{Block, MicroBlock}
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.acrylplatform.crypto._
import com.acrylplatform.features.BlockchainFeatures
import com.acrylplatform.lagonaki.mocks.TestBlock
import com.acrylplatform.settings._
import com.acrylplatform.transaction.Transaction

package object history {
  val MaxTransactionsPerBlockDiff = 10
  val MaxBlocksInMemory           = 5
  val DefaultBaseTarget           = 1000L
  val DefaultBlockchainSettings: BlockchainSettings = BlockchainSettings(
    addressSchemeCharacter = 'N',
    functionalitySettings = TestFunctionalitySettings.Enabled,
    genesisSettings = GenesisSettings.TESTNET,
    rewardsSettings = RewardsSettings.TESTNET
  )

  val config: Config          = ConfigFactory.load()
  val settings: AcrylSettings = AcrylSettings.fromRootConfig(config)

  val MicroblocksActivatedAt0BlockchainSettings: BlockchainSettings = DefaultBlockchainSettings.copy(
    functionalitySettings = DefaultBlockchainSettings.functionalitySettings.copy(preActivatedFeatures = Map(BlockchainFeatures.NG.id -> 0)))

  val DataAndMicroblocksActivatedAt0BlockchainSettings: BlockchainSettings = DefaultBlockchainSettings.copy(
    functionalitySettings = DefaultBlockchainSettings.functionalitySettings.copy(
      preActivatedFeatures = Map(BlockchainFeatures.NG.id -> 0, BlockchainFeatures.DataTransaction.id -> 0)))

  val TransfersV2ActivatedAt0BlockchainSettings: BlockchainSettings =
    DefaultBlockchainSettings.copy(
      functionalitySettings =
        DefaultBlockchainSettings.functionalitySettings.copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0)))

  val MicroblocksActivatedAt0AcrylSettings: AcrylSettings = settings.copy(blockchainSettings = MicroblocksActivatedAt0BlockchainSettings)

  val DataAndMicroblocksActivatedAt0AcrylSettings: AcrylSettings =
    settings.copy(blockchainSettings = DataAndMicroblocksActivatedAt0BlockchainSettings)

  val TransfersV2ActivatedAt0AcrylSettings: AcrylSettings = settings.copy(blockchainSettings = TransfersV2ActivatedAt0BlockchainSettings)

  val DefaultAcrylSettings: AcrylSettings = settings.copy(blockchainSettings = DefaultBlockchainSettings,
                                                          featuresSettings = settings.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false))

  val defaultSigner: KeyPair       = KeyPair(Array.fill(KeyLength)(0: Byte))
  val generationSignature: ByteStr = ByteStr(Array.fill(Block.GeneratorSignatureLength)(0: Byte))

  def buildBlockOfTxs(refTo: ByteStr, txs: Seq[Transaction]): Block = customBuildBlockOfTxs(refTo, txs, defaultSigner, 1, 0L)

  def customBuildBlockOfTxs(refTo: ByteStr,
                            txs: Seq[Transaction],
                            signer: KeyPair,
                            version: Byte,
                            timestamp: Long,
                            bTarget: Long = DefaultBaseTarget): Block =
    Block
      .buildAndSign(
        version = version,
        timestamp = timestamp,
        reference = refTo,
        consensusData = NxtLikeConsensusBlockData(baseTarget = bTarget, generationSignature = generationSignature),
        transactionData = txs,
        signer = signer,
        Set.empty
      )
      .explicitGet()

  def customBuildMicroBlockOfTxs(totalRefTo: ByteStr,
                                 prevTotal: Block,
                                 txs: Seq[Transaction],
                                 signer: KeyPair,
                                 version: Byte,
                                 ts: Long): (Block, MicroBlock) = {
    val newTotalBlock = customBuildBlockOfTxs(totalRefTo, prevTotal.transactionData ++ txs, signer, version, ts)
    val nonSigned = MicroBlock
      .buildAndSign(
        generator = signer,
        transactionData = txs,
        prevResBlockSig = prevTotal.uniqueId,
        totalResBlockSig = newTotalBlock.uniqueId
      )
      .explicitGet()
    (newTotalBlock, nonSigned)
  }

  def buildMicroBlockOfTxs(totalRefTo: ByteStr, prevTotal: Block, txs: Seq[Transaction], signer: KeyPair): (Block, MicroBlock) = {
    val newTotalBlock = buildBlockOfTxs(totalRefTo, prevTotal.transactionData ++ txs)
    val nonSigned = MicroBlock
      .buildAndSign(
        generator = signer,
        transactionData = txs,
        prevResBlockSig = prevTotal.uniqueId,
        totalResBlockSig = newTotalBlock.uniqueId
      )
      .explicitGet()
    (newTotalBlock, nonSigned)
  }

  def randomSig: ByteStr = TestBlock.randomOfLength(Block.BlockIdLength)

  def chainBlocks(txs: Seq[Seq[Transaction]]): Seq[Block] = {
    def chainBlocksR(refTo: ByteStr, txs: Seq[Seq[Transaction]]): Seq[Block] = txs match {
      case x :: xs =>
        val block = buildBlockOfTxs(refTo, x)
        block +: chainBlocksR(block.uniqueId, xs)
      case _ => Seq.empty
    }

    chainBlocksR(randomSig, txs)
  }

  def chainBaseAndMicro(totalRefTo: ByteStr, base: Transaction, micros: Seq[Seq[Transaction]]): (Block, Seq[MicroBlock]) =
    chainBaseAndMicro(totalRefTo, Seq(base), micros, defaultSigner, 3, 0L)

  def chainBaseAndMicro(totalRefTo: ByteStr,
                        base: Seq[Transaction],
                        micros: Seq[Seq[Transaction]],
                        signer: KeyPair,
                        version: Byte,
                        timestamp: Long): (Block, Seq[MicroBlock]) = {
    val block = customBuildBlockOfTxs(totalRefTo, base, signer, version, timestamp)
    val microBlocks = micros
      .foldLeft((block, Seq.empty[MicroBlock])) {
        case ((lastTotal, allMicros), txs) =>
          val (newTotal, micro) = customBuildMicroBlockOfTxs(totalRefTo, lastTotal, txs, signer, version, timestamp)
          (newTotal, allMicros :+ micro)
      }
      ._2
    (block, microBlocks)
  }

  def spoilSignature(b: Block): Block = b.copy(signerData = b.signerData.copy(signature = TestBlock.randomSignature()))
}
