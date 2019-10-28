package com.acrylplatform.state.extensions

import com.acrylplatform.database.LevelDBWriter
import com.acrylplatform.state.reader.CompositeBlockchain
import com.acrylplatform.state.{Blockchain, BlockchainUpdaterImpl}

trait BlockchainExtensions extends AddressTransactions.Prov[Blockchain] with Distributions.Prov[Blockchain] {
  override def addressTransactions(value: Blockchain): AddressTransactions = value match {
    case ldb: LevelDBWriter        => LevelDBWriter.addressTransactions(ldb)
    case bu: BlockchainUpdaterImpl => BlockchainUpdaterImpl.addressTransactions(bu)
    case c: CompositeBlockchain    => CompositeBlockchain.addressTransactions(c)
    case _                         => AddressTransactions.Empty
  }

  override def distributions(value: Blockchain): Distributions = value match {
    case ldb: LevelDBWriter        => LevelDBWriter.distributions(ldb)
    case bu: BlockchainUpdaterImpl => BlockchainUpdaterImpl.distributions(bu)
    case c: CompositeBlockchain    => CompositeBlockchain.distributions(c)
    case _                         => Distributions.Empty
  }
}
