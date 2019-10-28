package com.acrylplatform.transaction

import com.acrylplatform.account.{AddressScheme, PublicKey}
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.state.diffs.ProduceError._
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.lang.contract.DApp
import com.acrylplatform.lang.directives.values._
import com.acrylplatform.lang.script.{ContractScript, Script}
import com.acrylplatform.transaction.Asset.IssuedAsset
import com.acrylplatform.transaction.assets.SetAssetScriptTransaction
import org.scalacheck.Gen
import play.api.libs.json._

class SetAssetScriptTransactionSpecification extends GenericTransactionSpecification[SetAssetScriptTransaction] {
  def transactionParser: com.acrylplatform.transaction.TransactionParserFor[SetAssetScriptTransaction] = SetAssetScriptTransaction
  def updateProofs(tx: SetAssetScriptTransaction, p: Proofs): SetAssetScriptTransaction = {
    tx.copy(proofs = p)
  }
  def generator: Gen[((Seq[com.acrylplatform.transaction.Transaction], SetAssetScriptTransaction))] = setAssetScriptTransactionGen
  def assertTxs(first: SetAssetScriptTransaction, second: SetAssetScriptTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.version shouldEqual second.version
    first.asset shouldEqual second.asset
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
    first.script shouldEqual second.script
  }
  def jsonRepr: Seq[(JsValue, SetAssetScriptTransaction)] =
    Seq(
      (Json.parse(
         s"""{"type":15,"id":"3QmXxAi8k7vMxBZSYuzRz5MVuB77rPuZqorkyztcLaA8","sender":"3JZHQWq7BLGCquQ7epWRTb3FDpt1Dv8VLxC","senderPublicKey":"5k3gXC486CCFCwzUAgavH9JfPwmq9CbBZvTARnFujvgr","fee":78311891,"feeAssetId":null,"timestamp":1868142423132802425,"proofs":["5sRtXKcdDa","9Zfe5aw9D7rRR3nvU3QuAjCNT7pdwRXwvBFxHmdt2WtWwiEwffn","","3C","24jboCkAEFrsBKNh6z8FFyJP8YhejsrBwt7JdHVhiCk7DCc3Zxsc4g6PYG8tsLXmK",""],"version":1,"chainId":${AddressScheme.current.chainId},"assetId":"DUyJyszsWcmZG7q2Ctk1hisDeGBPB8dEzyU8Gs5V2j3n","script":"base64:AQkAAGcAAAACAHho/EXujJiPAJUhuPXZYac+rt2jYg=="}"""),
       SetAssetScriptTransaction
         .create(
           AddressScheme.current.chainId,
           PublicKey.fromBase58String("5k3gXC486CCFCwzUAgavH9JfPwmq9CbBZvTARnFujvgr").explicitGet(),
           IssuedAsset(ByteStr.decodeBase58("DUyJyszsWcmZG7q2Ctk1hisDeGBPB8dEzyU8Gs5V2j3n").get),
           Some(Script.fromBase64String("base64:AQkAAGcAAAACAHho/EXujJiPAJUhuPXZYac+rt2jYg==").explicitGet()),
           78311891L,
           1868142423132802425L,
           Proofs(
             Seq("5sRtXKcdDa",
                 "9Zfe5aw9D7rRR3nvU3QuAjCNT7pdwRXwvBFxHmdt2WtWwiEwffn",
                 "",
                 "3C",
                 "24jboCkAEFrsBKNh6z8FFyJP8YhejsrBwt7JdHVhiCk7DCc3Zxsc4g6PYG8tsLXmK",
                 "").map(ByteStr.decodeBase58(_).get))
         )
         .explicitGet()))
  def transactionName: String = "SetAssetScriptTransaction"

  property("issuer can`t make SetAssetScript tx when Script is Contract") {
    val accountA = PublicKey.fromBase58String("5k3gXC486CCFCwzUAgavH9JfPwmq9CbBZvTARnFujvgr").explicitGet()

    SetAssetScriptTransaction
      .create(
        AddressScheme.current.chainId,
        accountA,
        IssuedAsset(ByteStr.decodeBase58("DUyJyszsWcmZG7q2Ctk1hisDeGBPB8dEzyU8Gs5V2j3n").get),
        Some(ContractScript(V3, DApp(ByteStr.empty, List.empty, List.empty, None)).explicitGet()),
        1222,
        System.currentTimeMillis(),
        Proofs.empty
      ) should produce("not Contract")
  }
}
