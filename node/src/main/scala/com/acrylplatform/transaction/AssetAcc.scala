package com.acrylplatform.transaction

import com.acrylplatform.account.Address

case class AssetAcc(account: Address, assetId: Option[Asset])
