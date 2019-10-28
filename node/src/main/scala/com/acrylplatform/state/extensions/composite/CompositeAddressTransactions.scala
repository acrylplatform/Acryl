package com.acrylplatform.state.extensions.composite

import com.acrylplatform.account.Address
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.state.extensions.AddressTransactions
import com.acrylplatform.state.{Diff, Height}
import com.acrylplatform.transaction.{Transaction, TransactionParser}
import monix.reactive.Observable

private[state] final class CompositeAddressTransactions(baseProvider: AddressTransactions, height: Height, getDiff: () => Option[Diff])
    extends AddressTransactions {
  override def addressTransactionsObservable(address: Address,
                                             types: Set[TransactionParser],
                                             fromId: Option[ByteStr]): Observable[(Height, Transaction)] = {
    val fromDiff = for {
      diff                    <- getDiff().toIterable
      (height, tx, addresses) <- diff.transactions.values.toVector.reverse
    } yield (Height(height), tx, addresses)

    com.acrylplatform.state.addressTransactionsCompose(baseProvider, Observable.fromIterable(fromDiff))(address, types, fromId)
  }
}
