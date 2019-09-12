package com.acrylplatform.http

import com.acrylplatform.api.http.{ApiError, WithSettings}
import com.acrylplatform.lang.ValidationError
import com.acrylplatform.network._
import com.acrylplatform.transaction.Transaction
import com.acrylplatform.transaction.smart.script.trace.TracedResult
import com.acrylplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup

trait BroadcastRoute {
  self: WithSettings =>
  def utx: UtxPool
  def allChannels: ChannelGroup

  protected def doBroadcast(v: Either[ValidationError, Transaction]): TracedResult[ApiError, Transaction] = {
    val result = for {
      transaction <- TracedResult(v)
      isNew <- utx.putIfNew(transaction)
    } yield {
      if (isNew || settings.allowTxRebroadcasting) allChannels.broadcastTx(transaction, None)
      transaction
    }

    result.leftMap(ApiError.fromValidationError)
  }
}
