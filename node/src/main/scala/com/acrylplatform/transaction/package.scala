package com.acrylplatform

import com.acrylplatform.block.{Block, MicroBlock}
import com.acrylplatform.utils.base58Length

package object transaction {
  val AssetIdLength: Int       = com.acrylplatform.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)
  type DiscardedTransactions = Seq[Transaction]
  type DiscardedBlocks       = Seq[Block]
  type DiscardedMicroBlocks  = Seq[MicroBlock]
  type AuthorizedTransaction = Authorized with Transaction
}
