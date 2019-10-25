package com.acrylplatform.state.extensions

import com.acrylplatform.account.Address
import com.acrylplatform.block.Block.BlockId
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.state.Height
import com.acrylplatform.transaction.{Transaction, TransactionParser}
import monix.reactive.Observable

trait AddressTransactions {
  def addressTransactionsObservable(address: Address,
                                    types: Set[TransactionParser],
                                    fromId: Option[ByteStr] = None): Observable[(Height, Transaction)]
}

object AddressTransactions {
  def apply[T](value: T)(implicit ev: T => AddressTransactions): AddressTransactions = value

  trait Prov[T] {
    def addressTransactions(value: T): AddressTransactions
  }

  case object Empty extends AddressTransactions {
    override def addressTransactionsObservable(address: Address,
                                               types: Set[TransactionParser],
                                               fromId: Option[BlockId]): Observable[(Height, Transaction)] =
      Observable.empty
  }
}
