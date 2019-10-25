package com.acrylplatform.transaction

import com.acrylplatform.account.{AddressScheme, PublicKey}
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.transaction.Asset.IssuedAsset
import com.acrylplatform.transaction.assets.{IssueTransactionV1, ReissueTransactionV2}
import org.scalacheck.Gen
import play.api.libs.json._

class ReissueTransactionV2Specification extends GenericTransactionSpecification[ReissueTransactionV2] {

  def transactionParser: com.acrylplatform.transaction.TransactionParserFor[ReissueTransactionV2] = ReissueTransactionV2

  def updateProofs(tx: ReissueTransactionV2, p: Proofs): ReissueTransactionV2 = {
    tx.copy(proofs = p)
  }

  def assertTxs(first: ReissueTransactionV2, second: ReissueTransactionV2): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.version shouldEqual second.version
    first.quantity shouldEqual second.quantity
    first.reissuable shouldEqual second.reissuable
    first.asset shouldEqual second.asset
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }

  def generator: Gen[(Seq[com.acrylplatform.transaction.Transaction], ReissueTransactionV2)] =
    for {
      (sender, assetName, description, quantity, decimals, _, iFee, timestamp) <- issueParamGen
      fee                                                                      <- smallFeeGen
      reissuable                                                               <- Gen.oneOf(true, false)
    } yield {
      val issue = IssueTransactionV1.selfSigned(sender, assetName, description, quantity, decimals, reissuable = true, iFee, timestamp).explicitGet()
      val reissue1 = ReissueTransactionV2
        .selfSigned(AddressScheme.current.chainId, sender, IssuedAsset(issue.assetId), quantity, reissuable = reissuable, fee, timestamp)
        .explicitGet()
      (Seq(issue), reissue1)
    }

  def jsonRepr: Seq[(JsValue, ReissueTransactionV2)] =
    Seq(
      (Json.parse("""{
                       "type": 5,
                       "id": "7N8LyhjzA6iXQhkjUG7Bg59ydf3ahE8xXv2bvDBr9Q9s",
                       "sender": "3JTDzz1XbK7KeRJXGqpaRFraC92ebStimJ9",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000000,
                       "feeAssetId": null,
                       "timestamp": 1526287561757,
                       "proofs": [
                       "4DFEtUwJ9gjMQMuEXipv2qK7rnhhWEBqzpC3ZQesW1Kh8D822t62e3cRGWNU3N21r7huWnaty95wj2tZxYSvCfro"
                       ],
                       "version": 2,
                       "chainId": 75,
                       "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
                       "quantity": 100000000,
                       "reissuable": true
                    }
    """),
       ReissueTransactionV2
         .create(
           'K',
           PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
           IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get),
           100000000L,
           reissuable = true,
           100000000L,
           1526287561757L,
           Proofs(Seq(ByteStr.decodeBase58("4DFEtUwJ9gjMQMuEXipv2qK7rnhhWEBqzpC3ZQesW1Kh8D822t62e3cRGWNU3N21r7huWnaty95wj2tZxYSvCfro").get))
         )
         .right
         .get))

  def transactionName: String = "ReissueTransactionV2"
}
