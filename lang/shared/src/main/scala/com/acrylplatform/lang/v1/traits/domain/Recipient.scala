package com.acrylplatform.lang.v1.traits.domain

import com.acrylplatform.common.state.ByteStr

trait Recipient
object Recipient {
  case class Address(bytes: ByteStr) extends Recipient
  case class Alias(name: String)     extends Recipient
}
