package com.acrylplatform.protobuf

package object transaction {
  type PBOrder = com.acrylplatform.protobuf.transaction.ExchangeTransactionData.Order
  val PBOrder = com.acrylplatform.protobuf.transaction.ExchangeTransactionData.Order

  type VanillaOrder = com.acrylplatform.transaction.assets.exchange.Order
  val VanillaOrder = com.acrylplatform.transaction.assets.exchange.Order

  type PBTransaction = com.acrylplatform.protobuf.transaction.Transaction
  val PBTransaction = com.acrylplatform.protobuf.transaction.Transaction

  type PBSignedTransaction = com.acrylplatform.protobuf.transaction.SignedTransaction
  val PBSignedTransaction = com.acrylplatform.protobuf.transaction.SignedTransaction

  type VanillaTransaction = com.acrylplatform.transaction.Transaction
  val VanillaTransaction = com.acrylplatform.transaction.Transaction

  type VanillaSignedTransaction = com.acrylplatform.transaction.SignedTransaction

  type VanillaAssetId = com.acrylplatform.transaction.Asset
}
