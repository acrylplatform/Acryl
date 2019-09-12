package com.acrylplatform.lang.v1.traits.domain
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.lang.v1.traits.domain.Recipient.Address

case class ScriptAssetInfo(
    id:         ByteStr,
    quantity:   Long,
    decimals:   Int,
    issuer:     Address,
    issuerPk:   ByteStr,
    reissuable: Boolean,
    scripted:   Boolean,
    sponsored:  Boolean
)
