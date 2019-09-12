package com.acrylplatform.state.diffs

import com.acrylplatform.lang.ValidationError
import com.acrylplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.acrylplatform.transaction.smart.SetScriptTransaction

import scala.util.Right

object SetScriptTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: SetScriptTransaction): Either[ValidationError, Diff] = {
    val scriptOpt = tx.script
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
        scripts = Map(tx.sender.toAddress    -> scriptOpt),
        scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx),
        scriptsComplexity = DiffsCommon.countScriptsComplexity(blockchain, tx)
      ))
  }
}
