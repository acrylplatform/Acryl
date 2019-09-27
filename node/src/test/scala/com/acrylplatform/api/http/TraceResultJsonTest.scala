package com.acrylplatform.api.http

import com.acrylplatform.account.{Address, PublicKey}
import com.acrylplatform.api.http.ApiError.ScriptExecutionError
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.lang.v1.FunctionHeader.User
import com.acrylplatform.lang.v1.compiler.Terms.{CONST_LONG, CONST_STRING, FUNCTION_CALL}
import com.acrylplatform.lang.v1.evaluator.ScriptResult
import com.acrylplatform.lang.v1.traits.domain.DataItem.Lng
import com.acrylplatform.lang.v1.traits.domain.Recipient
import com.acrylplatform.transaction.Asset.Acryl
import com.acrylplatform.transaction.smart.InvokeScriptTransaction
import com.acrylplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.acrylplatform.transaction.smart.script.trace.{InvokeScriptTrace, TracedResult}
import com.acrylplatform.transaction.{Proofs, TxValidationError}
import org.scalatest.{Matchers, PropSpec}
import play.api.libs.json.Json

class TraceResultJsonTest extends PropSpec with Matchers {
  private val tx = (
    for {
      publicKey <- PublicKey.fromBase58String("9utotH1484Hb1WdAHuAKLjuGAmocPZg7jZDtnc35MuqT")
      address   <- Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB")
      proof     <- ByteStr.decodeBase58("4scXzk4WiKMXG8p7V6J2pmznNZCgMjADbbZPSDGg28YLMKgshBmNFNzgYg2TwfKN3wMtgLiNQB77iQQZkH3roUyJ").toEither
      tx <- InvokeScriptTransaction.create(
        sender = publicKey,
        dappAddress = address,
        fc = Some(FUNCTION_CALL(User("func"), List(CONST_STRING("param").explicitGet(), CONST_LONG(1)))),
        p = List(Payment(1, Acryl)),
        fee = 10000000,
        feeAssetId = Acryl,
        timestamp = 1111,
        proofs = Proofs(List(proof))
      )
    } yield tx
  ).explicitGet()

  property("suitable TracedResult json") {
    val trace = List(
      InvokeScriptTrace(
        tx.dAppAddressOrAlias,
        tx.funcCall,
        Right(
          ScriptResult(
            List(Lng("3FVV4W61poEVXEbFfPG1qfJhJxJ7Pk4M2To", 700000000)),
            List((Recipient.Address(tx.dAppAddressOrAlias.bytes), 1, None))
          ))
      ))

    val result = TracedResult(Right(tx), trace)

    Json.prettyPrint(result.json) shouldBe
      """{
        |  "senderPublicKey" : "9utotH1484Hb1WdAHuAKLjuGAmocPZg7jZDtnc35MuqT",
        |  "fee" : 10000000,
        |  "type" : 16,
        |  "version" : 1,
        |  "call" : {
        |    "function" : "func",
        |    "args" : [ {
        |      "type" : "string",
        |      "value" : "param"
        |    }, {
        |      "type" : "integer",
        |      "value" : 1
        |    } ]
        |  },
        |  "trace" : [ {
        |    "dApp" : "3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB",
        |    "function" : "func",
        |    "args" : [ "param", "1" ],
        |    "result" : {
        |      "data" : [ {
        |        "key" : "3FVV4W61poEVXEbFfPG1qfJhJxJ7Pk4M2To",
        |        "value" : "700000000"
        |      } ],
        |      "transfers" : [ {
        |        "address" : "3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB",
        |        "amount" : 1,
        |        "assetId" : null
        |      } ]
        |    }
        |  } ],
        |  "dApp" : "3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB",
        |  "sender" : "3JJrHPr6qmVjexdEGZCM54BbpMpuzhpzRSL",
        |  "feeAssetId" : null,
        |  "proofs" : [ "4scXzk4WiKMXG8p7V6J2pmznNZCgMjADbbZPSDGg28YLMKgshBmNFNzgYg2TwfKN3wMtgLiNQB77iQQZkH3roUyJ" ],
        |  "payment" : [ {
        |    "amount" : 1,
        |    "assetId" : null
        |  } ],
        |  "id" : "5qinNSAgmnTL4kVEULrU8P3nctKZRdvuF23BuHKYdJKC",
        |  "timestamp" : 1111
        |}""".stripMargin
  }

  property("suitable TracedResult error json") {
    val vars = List(
      "amount"     -> Right(CONST_LONG(12345)),
      "invocation" -> CONST_STRING("str")
    )
    val reason = "error reason"

    val trace = List(
      InvokeScriptTrace(
        tx.dAppAddressOrAlias,
        tx.funcCall,
        Left(TxValidationError.ScriptExecutionError(reason, vars, isAssetScript = false))
      ))
    val scriptExecutionError = ScriptExecutionError(tx, reason, isTokenScript = false)

    val result = TracedResult(Left(scriptExecutionError), trace)

    Json.prettyPrint(result.json) shouldBe
      """{
      |  "trace" : [ {
      |    "dApp" : "3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB",
      |    "function" : "func",
      |    "args" : [ "param", "1" ],
      |    "error" : {
      |      "type" : "Account",
      |      "vars" : [ {
      |        "name" : "amount",
      |        "value" : "12345"
      |      }, {
      |        "name" : "invocation",
      |        "value" : "str"
      |      } ],
      |      "reason" : "error reason"
      |    }
      |  } ],
      |  "error" : 306,
      |  "message" : "Error while executing account-script: error reason",
      |  "transaction" : {
      |    "senderPublicKey" : "9utotH1484Hb1WdAHuAKLjuGAmocPZg7jZDtnc35MuqT",
      |    "call" : {
      |      "function" : "func",
      |      "args" : [ {
      |        "type" : "string",
      |        "value" : "param"
      |      }, {
      |        "type" : "integer",
      |        "value" : 1
      |      } ]
      |    },
      |    "dApp" : "3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB",
      |    "sender" : "3JJrHPr6qmVjexdEGZCM54BbpMpuzhpzRSL",
      |    "feeAssetId" : null,
      |    "proofs" : [ "4scXzk4WiKMXG8p7V6J2pmznNZCgMjADbbZPSDGg28YLMKgshBmNFNzgYg2TwfKN3wMtgLiNQB77iQQZkH3roUyJ" ],
      |    "fee" : 10000000,
      |    "payment" : [ {
      |      "amount" : 1,
      |      "assetId" : null
      |    } ],
      |    "id" : "5qinNSAgmnTL4kVEULrU8P3nctKZRdvuF23BuHKYdJKC",
      |    "type" : 16,
      |    "version" : 1,
      |    "timestamp" : 1111
      |  }
      |}""".stripMargin
  }
}
