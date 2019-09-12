package com.acrylplatform.state.diffs

import cats.implicits._
import com.acrylplatform.account.Address
import com.acrylplatform.features.BlockchainFeatures
import com.acrylplatform.features.FeatureProvider._
import com.acrylplatform.lang.ValidationError
import com.acrylplatform.state._
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.TxValidationError
import com.acrylplatform.transaction.TxValidationError.GenericError
import com.acrylplatform.transaction.transfer._

import scala.util.{Right, Try}

object TransferTransactionDiff {
  def apply(blockchain: Blockchain, height: Int, blockTime: Long)(tx: TransferTransaction): Either[ValidationError, Diff] = {
    val sender = Address.fromPublicKey(tx.sender)

    val isSmartAsset = tx.feeAssetId match {
      case Acryl => false
      case asset @ IssuedAsset(_) =>
        blockchain
          .assetDescription(asset)
          .flatMap(_.script)
          .isDefined
    }

    for {
      recipient <- blockchain.resolveAlias(tx.recipient)
      _         <- Either.cond(!isSmartAsset, (), GenericError("Smart assets can't participate in TransferTransactions as a fee"))

      _ <- validateOverflow(blockchain, tx)
      portfolios = (tx.assetId match {
        case Acryl =>
          Map(sender -> Portfolio(-tx.amount, LeaseBalance.empty, Map.empty)).combine(
            Map(recipient -> Portfolio(tx.amount, LeaseBalance.empty, Map.empty))
          )
        case asset @ IssuedAsset(_) =>
          Map(sender -> Portfolio(0, LeaseBalance.empty, Map(asset -> -tx.amount))).combine(
            Map(recipient -> Portfolio(0, LeaseBalance.empty, Map(asset -> tx.amount)))
          )
      }).combine(
        tx.feeAssetId match {
          case Acryl => Map(sender -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty))
          case asset @ IssuedAsset(_) =>
            val senderPf = Map(sender -> Portfolio(0, LeaseBalance.empty, Map(asset -> -tx.fee)))
            if (height >= Sponsorship.sponsoredFeesSwitchHeight(blockchain)) {
              val sponsorPf = blockchain
                .assetDescription(asset)
                .collect {
                  case desc if desc.sponsorship > 0 =>
                    val feeInAcryl = Sponsorship.toAcryl(tx.fee, desc.sponsorship)
                    Map(desc.issuer.toAddress -> Portfolio(-feeInAcryl, LeaseBalance.empty, Map(asset -> tx.fee)))
                }
                .getOrElse(Map.empty)
              senderPf.combine(sponsorPf)
            } else senderPf
        }
      )
      assetIssued    = tx.assetId.fold(true)(blockchain.assetDescription(_).isDefined)
      feeAssetIssued = tx.feeAssetId.fold(true)(blockchain.assetDescription(_).isDefined)
      _ <- Either.cond(
        blockTime <= blockchain.settings.functionalitySettings.allowUnissuedAssetsUntil || (assetIssued && feeAssetIssued),
        (),
        GenericError(s"Unissued assets are not allowed after allowUnissuedAssetsUntil=${blockchain.settings.functionalitySettings.allowUnissuedAssetsUntil}")
      )
    } yield
      Diff(height,
        tx,
        portfolios,
        scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx),
        scriptsComplexity = DiffsCommon.countScriptsComplexity(blockchain, tx))
  }

  private def validateOverflow(blockchain: Blockchain, tx: TransferTransaction) = {
    if (blockchain.isFeatureActivated(BlockchainFeatures.Ride4DApps, blockchain.height)) {
      Right(()) // lets transaction validates itself
    } else {
      Try(Math.addExact(tx.fee, tx.amount))
        .fold(
          _ => TxValidationError.OverflowError.asLeft[Unit],
          _ => ().asRight[ValidationError]
        )
    }
  }
}
