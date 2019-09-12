package com.acrylplatform.lang.v1.traits.domain

import com.acrylplatform.common.state.ByteStr

case class APair(amountAsset: Option[ByteStr], priceAsset: Option[ByteStr])
