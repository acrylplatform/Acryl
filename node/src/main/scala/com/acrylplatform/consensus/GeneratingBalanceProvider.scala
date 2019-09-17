package com.acrylplatform.consensus

import com.acrylplatform.account.Address
import com.acrylplatform.block.Block
import com.acrylplatform.block.Block.BlockId
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.features.BlockchainFeatures
import com.acrylplatform.state.Blockchain

object GeneratingBalanceProvider {
  private val MinimalEffectiveBalanceForGenerator1: Long = 1000000000000L
  private val MinimalEffectiveBalanceForGenerator2: Long = 100000000000L
  private val MinimalEffectiveBalanceForGenerator3: Long = 10000000000L
  private val FirstDepth                                 = 50
  private val SecondDepth                                = 1000

  def isMiningAllowed(blockchain: Blockchain, height: Int, effectiveBalance: Long): Boolean = {
    val activatedOf1000 = blockchain.activatedFeatures.get(BlockchainFeatures.SmallerMinimalGeneratingBalance.id).exists(height >= _)
    val activatedOf100 = blockchain.activatedFeatures.get(BlockchainFeatures.MinimumGeneratingBalanceOf100.id).exists(height >= _)
    (!activatedOf100 && !activatedOf1000 && effectiveBalance >= MinimalEffectiveBalanceForGenerator1) ||
      (!activatedOf100 && activatedOf1000 && effectiveBalance >= MinimalEffectiveBalanceForGenerator2) ||
      (activatedOf100 && effectiveBalance >= MinimalEffectiveBalanceForGenerator3)
  }

  //noinspection ScalaStyle
  def isEffectiveBalanceValid(blockchain: Blockchain, height: Int, block: Block, effectiveBalance: Long): Boolean = {
    val activatedOf1000 = blockchain.activatedFeatures.get(BlockchainFeatures.SmallerMinimalGeneratingBalance.id).exists(height >= _)
    val activatedOf100 = blockchain.activatedFeatures.get(BlockchainFeatures.MinimumGeneratingBalanceOf100.id).exists(height >= _)
    block.timestamp < blockchain.settings.functionalitySettings.minimalGeneratingBalanceAfter || (block.timestamp >= blockchain.settings.functionalitySettings.minimalGeneratingBalanceAfter && effectiveBalance >= MinimalEffectiveBalanceForGenerator1) ||
      (activatedOf1000 && effectiveBalance >= MinimalEffectiveBalanceForGenerator2) || (activatedOf100 && effectiveBalance >= MinimalEffectiveBalanceForGenerator3)
  }

  def balance(blockchain: Blockchain, account: Address, blockId: BlockId = ByteStr.empty): Long = {
    val height =
      if (blockId.isEmpty) blockchain.height
      else blockchain.heightOf(blockId).getOrElse(throw new IllegalArgumentException(s"Invalid block ref: $blockId"))

    val depth = if (height >= blockchain.settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight) SecondDepth else FirstDepth
    blockchain.effectiveBalance(account, depth, blockId)
  }
}
