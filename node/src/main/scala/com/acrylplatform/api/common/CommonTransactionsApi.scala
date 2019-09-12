package com.acrylplatform.api.common

import com.acrylplatform.account.Address
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.lang.ValidationError
import com.acrylplatform.protobuf.transaction.VanillaTransaction
import com.acrylplatform.state.{Blockchain, Height}
import com.acrylplatform.transaction.Asset
import com.acrylplatform.transaction.smart.script.trace.TracedResult
import com.acrylplatform.utx.UtxPool
import com.acrylplatform.wallet.Wallet
import monix.reactive.Observable
import com.acrylplatform.state.diffs.FeeValidation
import com.acrylplatform.state.diffs.FeeValidation.FeeDetails

private[api] class CommonTransactionsApi(blockchain: Blockchain, utx: UtxPool, wallet: Wallet, broadcast: (VanillaTransaction, Boolean) => Unit) {

  def transactionsByAddress(address: Address, fromId: Option[ByteStr] = None): Observable[(Height, VanillaTransaction)] = {
    val iterator = blockchain.addressTransactions(address, Set.empty, fromId)
    Observable.fromIterator(iterator, () => iterator.close())
  }

  def transactionById(transactionId: ByteStr): Option[(Int, VanillaTransaction)] = {
    blockchain.transactionInfo(transactionId)
  }

  def unconfirmedTransactions(): Seq[VanillaTransaction] = {
    utx.all
  }

  def unconfirmedTransactionById(transactionId: ByteStr): Option[VanillaTransaction] = {
    utx.transactionById(transactionId)
  }

  def calculateFee(tx: VanillaTransaction): Either[ValidationError, (Asset, Long, Long)] = {
    FeeValidation
      .getMinFee(blockchain, blockchain.height, tx)
      .map {
        case FeeDetails(asset, _, feeInAsset, feeInAcryl) =>
          (asset, feeInAsset, feeInAcryl)
      }

  }

  def broadcastTransaction(tx: VanillaTransaction): TracedResult[ValidationError, VanillaTransaction] = {
    val result = for {
      isNew <- utx.putIfNew(tx)
      _ = broadcast(tx, isNew)
    } yield tx

    result
  }
}
