package com.acrylplatform.transaction

import com.acrylplatform.TransactionGen
import com.acrylplatform.account.PublicKey
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.transaction.lease.{LeaseCancelTransaction, LeaseCancelTransactionV1, LeaseCancelTransactionV2}
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.Json

class LeaseCancelTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Lease cancel serialization roundtrip") {
    forAll(leaseCancelGen) { tx: LeaseCancelTransaction =>
      val recovered = tx.builder.parseBytes(tx.bytes()).get.asInstanceOf[LeaseCancelTransaction]
      assertTxs(recovered, tx)
    }
  }

  property("Lease cancel serialization from TypedTransaction") {
    forAll(leaseCancelGen) { tx: LeaseCancelTransaction =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      assertTxs(recovered.asInstanceOf[LeaseCancelTransaction], tx)
    }
  }

  private def assertTxs(first: LeaseCancelTransaction, second: LeaseCancelTransaction): Unit = {
    first.leaseId shouldEqual second.leaseId
    first.fee shouldEqual second.fee
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
  }

  property("JSON format validation for LeaseCancelTransactionV1") {
    val js = Json.parse("""{
                       "type": 9,
                       "id": "7hmabbFS8a2z79a29pzZH1s8LHxrsEAnnLjJxNdZ1gGw",
                       "sender": "3JTDzz1XbK7KeRJXGqpaRFraC92ebStimJ9",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 1000000,
                       "feeAssetId": null,
                       "timestamp": 1526646300260,
                       "signature": "4T76AXcksn2ixhyMNu4m9UyY54M3HDTw5E2HqUsGV4phogs2vpgBcN5oncu4sbW4U3KU197yfHMxrc3kZ7e6zHG3",
                       "proofs": ["4T76AXcksn2ixhyMNu4m9UyY54M3HDTw5E2HqUsGV4phogs2vpgBcN5oncu4sbW4U3KU197yfHMxrc3kZ7e6zHG3"],
                       "version": 1,
                       "leaseId": "EXhjYjy8a1dURbttrGzfcft7cddDnPnoa3vqaBLCTFVY",
                       "chainId": null
                       }
    """)

    val tx = LeaseCancelTransactionV1
      .create(
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        ByteStr.decodeBase58("EXhjYjy8a1dURbttrGzfcft7cddDnPnoa3vqaBLCTFVY").get,
        1000000,
        1526646300260L,
        ByteStr.decodeBase58("4T76AXcksn2ixhyMNu4m9UyY54M3HDTw5E2HqUsGV4phogs2vpgBcN5oncu4sbW4U3KU197yfHMxrc3kZ7e6zHG3").get
      )
      .right
      .get

    js shouldEqual tx.json()
  }

  property("JSON format validation for LeaseCancelTransactionV2") {
    val js = Json.parse("""{
                        "type": 9,
                        "id": "9W6hQwAS7ddqT1s9rQE871qFZyJdTzZrdC1zSt7uFMFr",
                        "sender": "3JTDzz1XbK7KeRJXGqpaRFraC92ebStimJ9",
                        "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                        "fee": 1000000,
                        "feeAssetId":null,
                        "timestamp": 1526646300260,
                        "proofs": [
                        "3h5SQLbCzaLoTHUeoCjXUHB6qhNUfHZjQQVsWTRAgTGMEdK5aeULMVUfDq63J56kkHJiviYTDT92bLGc8ELrUgvi"
                        ],
                        "version": 2,
                        "leaseId": "DJWkQxRyJNqWhq9qSQpK2D4tsrct6eZbjSv3AH4PSha6",
                        "chainId": 75
                       }
    """)

    val tx = LeaseCancelTransactionV2
      .create(
        'K',
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        ByteStr.decodeBase58("DJWkQxRyJNqWhq9qSQpK2D4tsrct6eZbjSv3AH4PSha6").get,
        1000000,
        1526646300260L,
        Proofs(Seq(ByteStr.decodeBase58("3h5SQLbCzaLoTHUeoCjXUHB6qhNUfHZjQQVsWTRAgTGMEdK5aeULMVUfDq63J56kkHJiviYTDT92bLGc8ELrUgvi").get))
      )
      .right
      .get

    js shouldEqual tx.json()
  }

}
