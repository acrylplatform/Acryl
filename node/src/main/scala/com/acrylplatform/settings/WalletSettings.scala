package com.acrylplatform.settings

import java.io.File

import com.acrylplatform.common.state.ByteStr

case class WalletSettings(file: Option[File], password: Option[String], seed: Option[ByteStr])
