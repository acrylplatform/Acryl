package com.acrylplatform.generator.utils

import com.acrylplatform.generator.Preconditions.CreatedAccount
import com.acrylplatform.transaction.assets.IssueTransaction
import com.acrylplatform.transaction.lease.LeaseTransaction

object Universe {
  var Accounts: List[CreatedAccount] = Nil
  var IssuedAssets: List[IssueTransaction]        = Nil
  var Leases: List[LeaseTransaction]              = Nil
}
