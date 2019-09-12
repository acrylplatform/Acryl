package com.acrylplatform.state

import com.acrylplatform.block.Block.BlockId
import com.acrylplatform.consensus.nxt.NxtLikeConsensusBlockData

case class BlockMinerInfo(consensus: NxtLikeConsensusBlockData, timestamp: Long, blockId: BlockId)
