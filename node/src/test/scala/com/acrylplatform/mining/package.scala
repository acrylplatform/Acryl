package com.acrylplatform

import com.acrylplatform.state.{Blockchain, Diff}
import com.acrylplatform.transaction.Transaction

package object mining {
  private[mining] def createConstConstraint(maxSize: Long, transactionSize: => Long) = OneDimensionalMiningConstraint(
    maxSize,
    new com.acrylplatform.mining.TxEstimators.Fn {
      override def apply(b: Blockchain, t: Transaction, d: Diff) = transactionSize
      override val minEstimate                          = transactionSize
      override def toString(): String = s"const($transactionSize)"
    }
  )
}
