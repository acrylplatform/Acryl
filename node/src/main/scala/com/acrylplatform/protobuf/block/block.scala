package com.acrylplatform.protobuf

import com.acrylplatform

package object block {
  type PBBlock = com.acrylplatform.protobuf.block.Block
  val PBBlock: Block.type = com.acrylplatform.protobuf.block.Block

  type VanillaBlock = com.acrylplatform.block.Block
  val VanillaBlock: acrylplatform.block.Block.type = com.acrylplatform.block.Block
}
