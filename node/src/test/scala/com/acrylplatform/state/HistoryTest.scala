package com.acrylplatform.state

import com.acrylplatform.block.Block
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.crypto._
import com.acrylplatform.lagonaki.mocks.TestBlock

trait HistoryTest {
  val genesisBlock: Block = TestBlock.withReference(ByteStr(Array.fill(SignatureLength)(0: Byte)))

  def getNextTestBlock(blockchain: Blockchain): Block =
    TestBlock.withReference(blockchain.lastBlock.get.uniqueId)

  def getNextTestBlockWithVotes(blockchain: Blockchain, votes: Set[Short]): Block =
    TestBlock.withReferenceAndFeatures(blockchain.lastBlock.get.uniqueId, votes)
}
