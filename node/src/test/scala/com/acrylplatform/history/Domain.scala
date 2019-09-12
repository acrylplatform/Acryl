package com.acrylplatform.history

import com.acrylplatform.account.Address
import com.acrylplatform.block.Block
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.state._
import com.acrylplatform.transaction.BlockchainUpdater

case class Domain(blockchainUpdater: BlockchainUpdater with NG) {
  def effBalance(a: Address): Long          = blockchainUpdater.effectiveBalance(a, 1000)
  def appendBlock(b: Block)                 = blockchainUpdater.processBlock(b).explicitGet()
  def removeAfter(blockId: ByteStr)         = blockchainUpdater.removeAfter(blockId).explicitGet()
  def lastBlockId                           = blockchainUpdater.lastBlockId.get
  def portfolio(address: Address)           = blockchainUpdater.portfolio(address)
  def addressTransactions(address: Address) = blockchainUpdater.addressTransactions(address, Set.empty, 128, None)
  def carryFee                              = blockchainUpdater.carryFee
}
