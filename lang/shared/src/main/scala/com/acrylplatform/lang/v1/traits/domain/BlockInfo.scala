package com.acrylplatform.lang.v1.traits.domain
import com.acrylplatform.common.state.ByteStr

case class BlockInfo(timestamp: Long,
                     height: Int,
                     baseTarget: Long,
                     generationSignature: ByteStr,
                     generator: ByteStr,
                     generatorPublicKey: ByteStr)
