package com.acrylplatform.state.diffs

import java.nio.charset.StandardCharsets

import com.acrylplatform.account.{Address, AddressScheme, KeyPair}
import com.acrylplatform.lagonaki.mocks.TestBlock
import com.acrylplatform.transaction.GenesisTransaction
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.lang.directives.values.{Expression, V1}
import com.acrylplatform.lang.script.v1.ExprScript
import com.acrylplatform.lang.utils.compilerContext
import com.acrylplatform.lang.v1.compiler.ExpressionCompiler
import com.acrylplatform.lang.v1.parser.Parser
import com.acrylplatform.transaction.Asset.{IssuedAsset, Acryl}
import com.acrylplatform.transaction.assets.IssueTransactionV2
import com.acrylplatform.transaction.transfer.TransferTransactionV2
import org.scalatest.{Inside, PropSpec}

class TransactionValidationErrorPrintTest extends PropSpec with Inside {
  property("output transaction error should be easy to read") {
    val assetScript =
      s"""
        | let NETWORKBYTE = takeRight(toBytes(65), 1)
        |
        | match (tx) {
        |     # Only allow transfer transactions
        |     case t:TransferTransaction => {
        |         let txWithoutAttachment = dropRight(t.bodyBytes, 97) + takeRight(toBytes(0), 1)
        |
        |         let recipientPublicKeyAndSignature = t.attachment
        |         let recipientPublicKey = take(recipientPublicKeyAndSignature, 32)
        |         let recipientSignature = takeRight(recipientPublicKeyAndSignature, 64)
        |
        |         let recipientPublicKeyHash = take(keccak256(blake2b256(recipientPublicKey)), 20)
        |         let rpkWithVersionAndByte = takeRight(toBytes(1), 1) + NETWORKBYTE + recipientPublicKeyHash
        |         let checksum = take(keccak256(blake2b256(rpkWithVersionAndByte)), 4)
        |         let recipientAddressFromPublicKey = rpkWithVersionAndByte + checksum
        |         let recipientAddressFromTx = addressFromRecipient(t.recipient).bytes
        |         let recipientAddressStr = toBase58String(recipientAddressFromPublicKey)
        |         let big = base64'${"a" * 2048}'
        |
        |         if (big == big && recipientAddressFromPublicKey != recipientAddressFromTx) then throw(
        |             "Recipient address error:" + recipientAddressStr
        |             ) else {
        |           if (!sigVerify(txWithoutAttachment, recipientSignature, recipientPublicKey))
        |             then true
        |             else false
        |         }
        |     }
        |     case _ => throw("unexpected")
        | }
      """.stripMargin

    val untypedScript = Parser.parseExpr(assetScript).get.value

    val typedScript = ExprScript(ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), untypedScript).explicitGet()._1)
      .explicitGet()

    val seed    = Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet()
    val master  = Address.fromString("3JDhSx3oUBaUed3dRjVLDzQnFEyWN4kzGsT").explicitGet()
    val genesis = GenesisTransaction.create(master, 1000000000, 0).explicitGet()

    val issueTransaction = IssueTransactionV2
      .selfSigned(
        chainId = AddressScheme.current.chainId,
        sender = KeyPair(seed.bytes),
        name = "name".getBytes(StandardCharsets.UTF_8),
        description = "description".getBytes(StandardCharsets.UTF_8),
        quantity = 100,
        decimals = 0,
        reissuable = false,
        script = Some(typedScript),
        fee = 10000000,
        timestamp = 0
      )
      .explicitGet()

    val transferTransaction = TransferTransactionV2
      .selfSigned(
        assetId = IssuedAsset(issueTransaction.id()),
        sender = KeyPair(master.bytes),
        recipient = master,
        amount = 1,
        timestamp = 0,
        feeAssetId = Acryl,
        feeAmount = 10000000,
        attachment = Array[Byte]()
      )
      .explicitGet()

    assertDiffEi(
      Seq(TestBlock.create(Seq(genesis, issueTransaction))),
      TestBlock.create(Seq(transferTransaction))
    ) { error =>
      val expected = //regex because of changeable proof
        """Left\(TransactionValidationError\(cause = ScriptExecutionError\(error = Recipient address error:3ETL67tj9qVZivi2CkJvjC16Bt3S1kYoo6S, type = Asset, log =
            |	\$match0 = TransferTransaction\(
            |		recipient = Address\(
            |			bytes = base58'3JDhSx3oUBaUed3dRjVLDzQnFEyWN4kzGsT'
            |		\)
            |		timestamp = 0
            |		bodyBytes = base58'ZEhfTMAf87LyedAcGaowmpAc3jvkzbm6utXUuJWHXUymDf6Rdf5ctMEARdxjKykjyYgxmT42DG4aWAZX1PFiCo1FodCVo7XX2FfptfvebqCMUDUoY3WWcauhJB8MNa3bsn2ePWjvQFpf52U6M96buTSTCtFBTthjtns'
            |		assetId = base58'27K8J9joP31cXcRdo24p796rB5HmeumRB5ZqWJeu9Edn'
            |		feeAssetId = Unit
            |		amount = 1
            |		version = 2
            |		id = base58'6pq9gviqhtWRh2wrnWs8SFsMk8hhsaexJqGepJ9SKQtj'
            |		senderPublicKey = base58'9nn1LxqAsStxtjE2tGQn3daC99BjHTsXacfHE7n61z8K'
            |		attachment = base58''
            |		sender = Address\(
            |			bytes = base58'3JCUQMFc8xc8h5AhqYme6GZC8nMkTT4dD1A'
            |		\)
            |		fee = 10000000
            |	\)
            |	big = base64'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
            |	@xs = base58'11111112'
            |	@number = 1
            |	@xs = base58'111111128'
            |	@number = 1
            |	NETWORKBYTE = base58'28'
            |	t = TransferTransaction\(
            |		recipient = Address\(
            |			bytes = base58'3JDhSx3oUBaUed3dRjVLDzQnFEyWN4kzGsT'
            |		\)
            |		timestamp = 0
            |		bodyBytes = base58'ZEhfTMAf87LyedAcGaowmpAc3jvkzbm6utXUuJWHXUymDf6Rdf5ctMEARdxjKykjyYgxmT42DG4aWAZX1PFiCo1FodCVo7XX2FfptfvebqCMUDUoY3WWcauhJB8MNa3bsn2ePWjvQFpf52U6M96buTSTCtFBTthjtns'
            |		assetId = base58'27K8J9joP31cXcRdo24p796rB5HmeumRB5ZqWJeu9Edn'
            |		feeAssetId = Unit
            |		amount = 1
            |		version = 2
            |		id = base58'6pq9gviqhtWRh2wrnWs8SFsMk8hhsaexJqGepJ9SKQtj'
            |		senderPublicKey = base58'9nn1LxqAsStxtjE2tGQn3daC99BjHTsXacfHE7n61z8K'
            |		attachment = base58''
            |		sender = Address\(
            |			bytes = base58'3JCUQMFc8xc8h5AhqYme6GZC8nMkTT4dD1A'
            |		\)
            |		fee = 10000000
            |	\)
            |	recipientPublicKeyAndSignature = base58''
            |	recipientPublicKey = base58''
            |	recipientPublicKeyHash = base58'3aDy5kHaDeXWfQwMrBCRvd6r7gzg'
            |	rpkWithVersionAndByte = base58'LnRWhBAv33fowYCbMHxS8XcsCVYpU'
            |	checksum = base58'58J8Gz'
            |	recipientAddressFromPublicKey = base58'3ETL67tj9qVZivi2CkJvjC16Bt3S1kYoo6S'
            |	recipientAddressFromTx = base58'3JDhSx3oUBaUed3dRjVLDzQnFEyWN4kzGsT'
            |	@a = base58'3ETL67tj9qVZivi2CkJvjC16Bt3S1kYoo6S'
            |	@b = base58'3JDhSx3oUBaUed3dRjVLDzQnFEyWN4kzGsT'
            |	@p = FALSE
            |	recipientAddressStr = "3ETL67tj9qVZivi2CkJvjC16Bt3S1kYoo6S"
            |\),
            |tx = \{
            |  "senderPublicKey" : "9nn1LxqAsStxtjE2tGQn3daC99BjHTsXacfHE7n61z8K",
            |  "amount" : 1,
            |  "fee" : 10000000,
            |  "type" : 4,
            |  "version" : 2,
            |  "attachment" : "",
            |  "sender" : "3JCUQMFc8xc8h5AhqYme6GZC8nMkTT4dD1A",
            |  "feeAssetId" : null,
            |  "proofs" : \[ "\w+" ],
            |  "assetId" : "27K8J9joP31cXcRdo24p796rB5HmeumRB5ZqWJeu9Edn",
            |  "recipient" : "3JDhSx3oUBaUed3dRjVLDzQnFEyWN4kzGsT",
            |  "feeAsset" : null,
            |  "id" : "6pq9gviqhtWRh2wrnWs8SFsMk8hhsaexJqGepJ9SKQtj",
            |  "timestamp" : 0
            |}\)\)""".stripMargin.r

      error.toString should fullyMatch regex expected
    }
  }
}
