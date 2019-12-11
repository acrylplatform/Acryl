package com.acrylplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.acrylplatform.account.KeyPair
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.it.NodeConfigs.Default
import com.acrylplatform.it.api.SyncHttpApi._
import com.acrylplatform.it.transactions.BaseTransactionSuite
import com.acrylplatform.it.util._
import com.acrylplatform.state.Sponsorship
import com.acrylplatform.transaction.assets.IssueTransactionV1
import org.scalatest.CancelAfterFailure

class CustomFeeTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  import CustomFeeTransactionSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private val transferFee = 100000
  private val assetFee    = 1.acryl
  private val assetToken  = 100

  test("make transfer with sponsored asset") {
    val (balance1, eff1) = notMiner.accountBalances(senderAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)
    val (balance3, eff3) = notMiner.accountBalances(minerAddress)

    val req           = createSignedIssueRequest(assetTx)
    val issuedAssetId = notMiner.signedIssue(req).id
    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

    val sponsorAssetId = notMiner.sponsorAsset(senderAddress, issuedAssetId, assetToken, assetFee).id
    assert(!sponsorAssetId.isEmpty)
    nodes.waitForHeightAriseAndTxPresent(sponsorAssetId)

    val fees = 2 * assetFee
    notMiner.assertBalances(senderAddress, balance1 - fees, eff1 - fees)
    notMiner.assertAssetBalance(senderAddress, issuedAssetId, defaultAssetQuantity)

    // until `feature-check-blocks-period` blocks have been mined, sponsorship does not occur
    val unsponsoredId = notMiner.transfer(senderAddress, secondAddress, 1, transferFee, Some(issuedAssetId), Some(issuedAssetId)).id
    nodes.waitForHeightAriseAndTxPresent(unsponsoredId)
    notMiner.assertBalances(senderAddress, balance1 - fees, eff1 - fees)
    notMiner.assertBalances(secondAddress, balance2, eff2)
    notMiner.assertBalances(minerAddress, balance3 + fees, eff3 + fees)

    notMiner.assertAssetBalance(senderAddress, issuedAssetId, defaultAssetQuantity - transferFee - 1)
    notMiner.assertAssetBalance(secondAddress, issuedAssetId, 1)
    notMiner.assertAssetBalance(minerAddress, issuedAssetId, transferFee)

    // after `feature-check-blocks-period` asset fees should be sponsored
    nodes.waitForSameBlockHeadersAt(featureCheckBlocksPeriod)
    val sponsoredId = notMiner.transfer(senderAddress, secondAddress, 1, transferFee, Some(issuedAssetId), Some(issuedAssetId)).id
    nodes.waitForHeightAriseAndTxPresent(sponsoredId)

    val sponsorship = Sponsorship.toAcryl(transferFee, assetToken)
    notMiner.assertBalances(senderAddress, balance1 - fees - sponsorship, eff1 - fees - sponsorship)
    notMiner.assertBalances(secondAddress, balance2, eff2)
    notMiner.assertBalances(minerAddress, balance3 + fees + sponsorship, balance3 + fees + sponsorship)

    notMiner.assertAssetBalance(senderAddress, issuedAssetId, defaultAssetQuantity - transferFee - 2)
    notMiner.assertAssetBalance(secondAddress, issuedAssetId, 2)
    notMiner.assertAssetBalance(minerAddress, issuedAssetId, transferFee)
  }

}

object CustomFeeTransactionSuite {
  private val minerAddress             = Default.head.getString("address")
  private val senderAddress            = Default(2).getString("address")
  private val defaultAssetQuantity     = 999999999999L
  private val featureCheckBlocksPeriod = 13

  private val seed = Default(2).getString("account-seed")
  private val pk   = KeyPair.fromSeed(seed).explicitGet()
  val assetTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = pk,
      name = "asset".getBytes("UTF-8"),
      description = "asset description".getBytes("UTF-8"),
      quantity = defaultAssetQuantity,
      decimals = 2,
      reissuable = false,
      fee = 1.acryl,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val assetId: ByteStr = assetTx.id()

  private val minerConfig = ConfigFactory.parseString(s"""
                                                         | acryl.fees.transfer.$assetId = 100000
                                                         | acryl.blockchain.custom.functionality {
                                                         |   feature-check-blocks-period = $featureCheckBlocksPeriod
                                                         |   blocks-for-feature-activation = $featureCheckBlocksPeriod
                                                         |   pre-activated-features = { 7 = 0 }
                                                         |}""".stripMargin)

  private val notMinerConfig = ConfigFactory.parseString("acryl.miner.enable=no").withFallback(minerConfig)

  val Configs: Seq[Config] = Seq(
    minerConfig.withFallback(Default.head),
    notMinerConfig.withFallback(Default(1)),
    notMinerConfig.withFallback(Default(2))
  )

}
