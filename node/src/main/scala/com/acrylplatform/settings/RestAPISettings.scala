package com.acrylplatform.settings

case class RestAPISettings(enable: Boolean,
                           bindAddress: String,
                           port: Int,
                           apiKeyHash: String,
                           cors: Boolean,
                           https: Boolean,
                           certificateFile: String,
                           apiKeyDifferentHost: Boolean,
                           transactionsByAddressLimit: Int,
                           distributionAddressLimit: Int,
                           allowTxRebroadcasting: Boolean,
                           maxBlocksPerRequest: Int)
