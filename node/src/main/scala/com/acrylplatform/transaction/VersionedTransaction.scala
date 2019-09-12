package com.acrylplatform.transaction

trait VersionedTransaction {
  def version: Byte
}
