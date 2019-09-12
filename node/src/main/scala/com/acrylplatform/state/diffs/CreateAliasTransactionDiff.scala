package com.acrylplatform.state.diffs

import com.acrylplatform.features.BlockchainFeatures
import com.acrylplatform.features.FeatureProvider._
import com.acrylplatform.lang.ValidationError
import com.acrylplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.acrylplatform.transaction.CreateAliasTransaction
import com.acrylplatform.transaction.TxValidationError.GenericError

import scala.util.Right

object CreateAliasTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: CreateAliasTransaction): Either[ValidationError, Diff] =
    if (blockchain.isFeatureActivated(BlockchainFeatures.DataTransaction, height) && !blockchain.canCreateAlias(tx.alias))
      Left(GenericError("Alias already claimed"))
    else
      Right(
        Diff(
          height = height,
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
          aliases = Map(tx.alias -> tx.sender.toAddress),
          scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx),
          scriptsComplexity = DiffsCommon.countScriptsComplexity(blockchain, tx)
        ))
}
