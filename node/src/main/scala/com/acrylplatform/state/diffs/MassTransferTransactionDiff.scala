package com.acrylplatform.state.diffs

import cats.implicits._
import com.acrylplatform.account.Address
import com.acrylplatform.lang.ValidationError
import com.acrylplatform.state._
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.TxValidationError.{GenericError, Validation}
import com.acrylplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.acrylplatform.transaction.transfer._

object MassTransferTransactionDiff {

  def apply(blockchain: Blockchain, blockTime: Long, height: Int)(tx: MassTransferTransaction): Either[ValidationError, Diff] = {
    def parseTransfer(xfer: ParsedTransfer): Validation[(Map[Address, Portfolio], Long)] = {
      for {
        recipientAddr <- blockchain.resolveAlias(xfer.address)
        portfolio = tx.assetId
          .fold(Map(recipientAddr -> Portfolio(xfer.amount, LeaseBalance.empty, Map.empty))) { asset =>
            Map(recipientAddr -> Portfolio(0, LeaseBalance.empty, Map(asset -> xfer.amount)))
          }
      } yield (portfolio, xfer.amount)
    }
    val portfoliosEi = tx.transfers.traverse(parseTransfer)

    portfoliosEi.flatMap { list: List[(Map[Address, Portfolio], Long)] =>
      val sender   = Address.fromPublicKey(tx.sender)
      val foldInit = (Map(sender -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)), 0L)
      val (recipientPortfolios, totalAmount) = list.fold(foldInit) { (u, v) =>
        (u._1 combine v._1, u._2 + v._2)
      }
      val completePortfolio =
        recipientPortfolios
          .combine(
            tx.assetId
              .fold(Map(sender -> Portfolio(-totalAmount, LeaseBalance.empty, Map.empty))) { asset =>
                Map(sender -> Portfolio(0, LeaseBalance.empty, Map(asset -> -totalAmount)))
              }
          )

      val assetIssued = tx.assetId match {
        case Acryl                  => true
        case asset @ IssuedAsset(_) => blockchain.assetDescription(asset).isDefined
      }

      Either.cond(
        assetIssued,
        Diff(height,
          tx,
          completePortfolio,
          scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx),
          scriptsComplexity = DiffsCommon.countScriptsComplexity(blockchain, tx)),
        GenericError(s"Attempt to transfer a nonexistent asset")
      )
    }
  }
}
