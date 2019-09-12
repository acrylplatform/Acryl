package com.acrylplatform.extensions

import akka.actor.ActorSystem
import com.acrylplatform.account.Address
import com.acrylplatform.settings.AcrylSettings
import com.acrylplatform.state.Blockchain
import com.acrylplatform.transaction.{Asset, Transaction}
import com.acrylplatform.utils.Time
import com.acrylplatform.utx.UtxPool
import com.acrylplatform.wallet.Wallet
import monix.reactive.Observable

trait Context {
  def settings: AcrylSettings
  def blockchain: Blockchain
  def time: Time
  def wallet: Wallet
  def utx: UtxPool
  def broadcastTx(tx: Transaction): Unit
  def spendableBalanceChanged: Observable[(Address, Asset)]
  def actorSystem: ActorSystem
}
