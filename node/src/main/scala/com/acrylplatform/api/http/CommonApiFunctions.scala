package com.acrylplatform.api.http

import akka.http.scaladsl.server.Directive1
import com.acrylplatform.api.http.ApiError.{BlockDoesNotExist, InvalidSignature}
import com.acrylplatform.block.Block
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.state.Blockchain
import com.acrylplatform.transaction.TransactionParsers

trait CommonApiFunctions { this: ApiRoute =>
  protected[api] def withBlock(blockchain: Blockchain, encodedSignature: String): Directive1[Block] =
    if (encodedSignature.length > TransactionParsers.SignatureStringLength) complete(InvalidSignature)
    else {
      ByteStr
        .decodeBase58(encodedSignature)
        .toOption
        .toRight(InvalidSignature)
        .flatMap(s => blockchain.blockById(s).toRight(BlockDoesNotExist)) match {
        case Right(b) => provide(b)
        case Left(e)  => complete(e)
      }
    }
}
