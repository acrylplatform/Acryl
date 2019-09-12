package com.acrylplatform

import com.acrylplatform.settings.WalletSettings
import com.acrylplatform.wallet.Wallet

trait TestWallet {
  protected val testWallet: Wallet = {
    val wallet = Wallet(WalletSettings(None, Some("123"), None))
    wallet.generateNewAccounts(10)
    wallet
  }
}
