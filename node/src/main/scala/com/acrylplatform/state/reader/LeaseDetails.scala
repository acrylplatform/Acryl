package com.acrylplatform.state.reader

import com.acrylplatform.account.{PublicKey, AddressOrAlias}

case class LeaseDetails(sender: PublicKey, recipient: AddressOrAlias, height: Int, amount: Long, isActive: Boolean)
