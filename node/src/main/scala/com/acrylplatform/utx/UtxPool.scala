package com.acrylplatform.utx

import com.acrylplatform.account.Address
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.lang.ValidationError
import com.acrylplatform.mining.MultiDimensionalMiningConstraint
import com.acrylplatform.state.Portfolio
import com.acrylplatform.transaction._
import com.acrylplatform.transaction.smart.script.trace.TracedResult

import scala.concurrent.duration.Duration

trait UtxPool extends AutoCloseable {
  def putIfNew(tx: Transaction, verify: Boolean = true): TracedResult[ValidationError, Boolean]

  def removeAll(txs: Traversable[Transaction]): Unit

  def spendableBalance(addr: Address, assetId: Asset): Long

  def pessimisticPortfolio(addr: Address): Portfolio

  def all: Seq[Transaction]

  def size: Int

  def transactionById(transactionId: ByteStr): Option[Transaction]

  def packUnconfirmed(rest: MultiDimensionalMiningConstraint, maxPackTime: Duration): (Seq[Transaction], MultiDimensionalMiningConstraint)
}
