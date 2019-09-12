package com.acrylplatform.state.diffs

import com.acrylplatform.lang.ValidationError
import com.acrylplatform.state.{Diff, LeaseBalance, Portfolio}
import com.acrylplatform.transaction.TxValidationError.GenericError
import com.acrylplatform.transaction.GenesisTransaction

import scala.util.{Left, Right}

object GenesisTransactionDiff {
  def apply(height: Int)(tx: GenesisTransaction): Either[ValidationError, Diff] = {
    if (height != 1) Left(GenericError("GenesisTransaction cannot appear in non-initial block"))
    else
      Right(Diff(height = height, tx = tx, portfolios = Map(tx.recipient -> Portfolio(balance = tx.amount, LeaseBalance.empty, assets = Map.empty))))
  }
}
