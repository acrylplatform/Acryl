package com.acrylplatform.transaction
import com.acrylplatform.account.PublicKey

trait Authorized {
  val sender: PublicKey
}
