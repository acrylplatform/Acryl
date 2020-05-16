package com.acrylplatform.features

case class BlockchainFeature private (id: Short, description: String)

object BlockchainFeatures {

  val SmallerMinimalGeneratingBalance = BlockchainFeature(1, "Minimum Generating Balance of 1000 ACRYL")
  val NG                              = BlockchainFeature(2, "NG Protocol")
  val MassTransfer                    = BlockchainFeature(3, "Mass Transfer Transaction")
  val SmartAccounts                   = BlockchainFeature(4, "Smart Accounts")
  val DataTransaction                 = BlockchainFeature(5, "Data Transaction")
  val BurnAnyTokens                   = BlockchainFeature(6, "Burn Any Tokens")
  val FeeSponsorship                  = BlockchainFeature(7, "Fee Sponsorship")
  val FairPoS                         = BlockchainFeature(8, "Fair PoS")
  val SmartAssets                     = BlockchainFeature(9, "Smart Assets")
  val SmartAccountTrading             = BlockchainFeature(10, "Smart Account Trading")
  val Ride4DApps                      = BlockchainFeature(11, "RIDE 4 DAPPS")
  val OrderV3                         = BlockchainFeature(12, "Order Version 3")
  val ReduceNFTFee                    = BlockchainFeature(13, "Reduce NFT fee")
  val MinimumGeneratingBalanceOf100   = BlockchainFeature(14, "Minimum Generating Balance of 100 ACRYL")
  val ChangeMinimalFees               = BlockchainFeature(15, "Change minimal tx fees")
  val BlockReward                     = BlockchainFeature(16, "Block Reward")

  // When next fork-parameter is created, you must replace all uses of the DummyFeature with the new one.
  val DummyFeature = BlockchainFeature(-1, "Non Votable!")

  private[this] val FeaturesMap = Seq(
    SmallerMinimalGeneratingBalance,
    NG,
    MassTransfer,
    SmartAccounts,
    DataTransaction,
    BurnAnyTokens,
    FeeSponsorship,
    FairPoS,
    SmartAccountTrading,
    SmartAssets,
    Ride4DApps,
    OrderV3,
    ReduceNFTFee,
    MinimumGeneratingBalanceOf100,
    ChangeMinimalFees,
    BlockReward
  ).map(f => f.id -> f).toMap

  val implemented: Set[Short] = FeaturesMap.keySet

  def feature(id: Short): Option[BlockchainFeature] = FeaturesMap.get(id)
}
