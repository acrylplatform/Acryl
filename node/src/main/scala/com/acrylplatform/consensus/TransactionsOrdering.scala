package com.acrylplatform.consensus

import com.acrylplatform.transaction.Asset.Acryl
import com.acrylplatform.transaction.Transaction

object TransactionsOrdering {
  trait AcrylOrdering extends Ordering[Transaction] {
    def txTimestampOrder(ts: Long): Long
    private def orderBy(t: Transaction): (Double, Long, Long) = {
      val size        = t.bytes().length
      val byFee       = if (t.assetFee._1 != Acryl) 0 else -t.assetFee._2
      val byTimestamp = txTimestampOrder(t.timestamp)

      (byFee.toDouble / size.toDouble, byFee, byTimestamp)
    }
    override def compare(first: Transaction, second: Transaction): Int = {
      implicitly[Ordering[(Double, Long, Long)]].compare(orderBy(first), orderBy(second))
    }
  }

  object InBlock extends AcrylOrdering {
    // sorting from network start
    override def txTimestampOrder(ts: Long): Long = -ts
  }

  object InUTXPool extends AcrylOrdering {
    override def txTimestampOrder(ts: Long): Long = ts
  }
}
