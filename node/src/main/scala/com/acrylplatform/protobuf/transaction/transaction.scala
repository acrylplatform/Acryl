package com.acrylplatform.protobuf

import com.acrylplatform
import com.acrylplatform.transaction.assets.exchange.Order

package object transaction {
  type PBOrder = com.acrylplatform.protobuf.transaction.ExchangeTransactionData.Order
  val PBOrder: ExchangeTransactionData.Order.type = com.acrylplatform.protobuf.transaction.ExchangeTransactionData.Order

  type VanillaOrder = com.acrylplatform.transaction.assets.exchange.Order
  val VanillaOrder: Order.type = com.acrylplatform.transaction.assets.exchange.Order

  type PBTransaction = com.acrylplatform.protobuf.transaction.Transaction
  val PBTransaction: Transaction.type = com.acrylplatform.protobuf.transaction.Transaction

  type PBSignedTransaction = com.acrylplatform.protobuf.transaction.SignedTransaction
  val PBSignedTransaction: SignedTransaction.type = com.acrylplatform.protobuf.transaction.SignedTransaction

  type VanillaTransaction = com.acrylplatform.transaction.Transaction
  val VanillaTransaction: acrylplatform.transaction.Transaction.type = com.acrylplatform.transaction.Transaction

  type VanillaSignedTransaction = com.acrylplatform.transaction.SignedTransaction

  type VanillaAssetId = com.acrylplatform.transaction.Asset
}
