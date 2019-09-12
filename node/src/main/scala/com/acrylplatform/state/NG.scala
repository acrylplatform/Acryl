package com.acrylplatform.state

import com.acrylplatform.block.Block.BlockId
import com.acrylplatform.block.MicroBlock
import com.acrylplatform.common.state.ByteStr

trait NG extends Blockchain {
  def microBlock(id: ByteStr): Option[MicroBlock]

  def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo]

  def lastPersistedBlockIds(count: Int): Seq[BlockId]

  def microblockIds: Seq[BlockId]
}
