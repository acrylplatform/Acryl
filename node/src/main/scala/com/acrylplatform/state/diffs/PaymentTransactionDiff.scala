package com.acrylplatform.state.diffs

import cats.implicits._
import com.acrylplatform.account.Address
import com.acrylplatform.lang.ValidationError
import com.acrylplatform.settings.FunctionalitySettings
import com.acrylplatform.state.{Diff, LeaseBalance, Portfolio}
import com.acrylplatform.transaction.PaymentTransaction
import com.acrylplatform.transaction.TxValidationError.GenericError

import scala.util.{Left, Right}

object PaymentTransactionDiff {

  def apply(settings: FunctionalitySettings, height: Int, blockTime: Long)(tx: PaymentTransaction): Either[ValidationError, Diff] = {

    if (height > settings.blockVersion3AfterHeight) {
      Left(GenericError(s"Payment transaction is deprecated after h=${settings.blockVersion3AfterHeight}"))
    } else {
      Right(
        Diff(
          height = height,
          tx = tx,
          portfolios = Map(tx.recipient -> Portfolio(balance = tx.amount, LeaseBalance.empty, assets = Map.empty)) combine Map(
            Address.fromPublicKey(tx.sender) -> Portfolio(
              balance = -tx.amount - tx.fee,
              LeaseBalance.empty,
              assets = Map.empty
            ))
        ))
    }
  }
}
