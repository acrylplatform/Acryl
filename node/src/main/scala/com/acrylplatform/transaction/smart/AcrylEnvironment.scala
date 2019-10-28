package com.acrylplatform.transaction.smart

import com.acrylplatform.account.AddressOrAlias
import com.acrylplatform.block.BlockHeader
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.EitherExt2
import com.acrylplatform.lang.v1.traits._
import com.acrylplatform.lang.v1.traits.domain.Recipient._
import com.acrylplatform.lang.v1.traits.domain.Tx.ScriptTransfer
import com.acrylplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.acrylplatform.state._
import com.acrylplatform.transaction.Asset.IssuedAsset
import com.acrylplatform.transaction.assets.exchange.Order
import com.acrylplatform.transaction.{Asset, Transaction}
import monix.eval.Coeval
import shapeless._

object AcrylEnvironment {
  type In = Transaction :+: Order :+: ScriptTransfer :+: CNil
}

class AcrylEnvironment(nByte: Byte, in: Coeval[AcrylEnvironment.In], h: Coeval[Int], blockchain: Blockchain, address: Coeval[ByteStr])
    extends Environment {
  override def height: Long = h()

  override def inputEntity: Environment.InputEntity = {
    in.apply()
      .map(InputPoly)
  }

  override def transactionById(id: Array[Byte]): Option[Tx] =
    blockchain
      .transactionInfo(ByteStr(id))
      .map(_._2)
      .map(tx => RealTransactionWrapper(tx, Some(id)))

  override def transferTransactionById(id: Array[Byte]): Option[Tx] =
    blockchain
      .transferById(id)
      .map(t => RealTransactionWrapper.mapTransferTx(t._2))

  override def data(recipient: Recipient, key: String, dataType: DataType): Option[Any] = {
    for {
      address <- recipient match {
        case Address(bytes) =>
          com.acrylplatform.account.Address
            .fromBytes(bytes.arr)
            .toOption
        case Alias(name) =>
          com.acrylplatform.account.Alias
            .create(name)
            .flatMap(blockchain.resolveAlias)
            .toOption
      }
      data <- blockchain
        .accountData(address, key)
        .map((_, dataType))
        .flatMap {
          case (IntegerDataEntry(_, value), DataType.Long)     => Some(value)
          case (BooleanDataEntry(_, value), DataType.Boolean)  => Some(value)
          case (BinaryDataEntry(_, value), DataType.ByteArray) => Some(ByteStr(value.arr))
          case (StringDataEntry(_, value), DataType.String)    => Some(value)
          case _                                               => None
        }
    } yield data
  }
  override def resolveAlias(name: String): Either[String, Recipient.Address] =
    blockchain
      .resolveAlias(com.acrylplatform.account.Alias.create(name).explicitGet())
      .left
      .map(_.toString)
      .right
      .map(a => Recipient.Address(ByteStr(a.bytes.arr)))

  override def chainId: Byte = nByte

  override def accountBalanceOf(addressOrAlias: Recipient, maybeAssetId: Option[Array[Byte]]): Either[String, Long] = {
    (for {
      aoa <- addressOrAlias match {
        case Address(bytes) => AddressOrAlias.fromBytes(bytes.arr, position = 0).map(_._1)
        case Alias(name)    => com.acrylplatform.account.Alias.create(name)
      }
      address <- blockchain.resolveAlias(aoa)
      balance = blockchain.balance(address, Asset.fromCompatId(maybeAssetId.map(ByteStr(_))))
    } yield balance).left.map(_.toString)
  }

  override def transactionHeightById(id: Array[Byte]): Option[Long] =
    blockchain.transactionHeight(ByteStr(id)).map(_.toLong)

  override def tthis: Address = Recipient.Address(address())

  override def assetInfoById(id: Array[Byte]): Option[domain.ScriptAssetInfo] = {
    blockchain.assetDescription(IssuedAsset(id)).map { assetDesc =>
      ScriptAssetInfo(
        id = id,
        quantity = assetDesc.totalVolume.toLong,
        decimals = assetDesc.decimals,
        issuer = Address(assetDesc.issuer.toAddress.bytes),
        issuerPk = assetDesc.issuer,
        reissuable = assetDesc.reissuable,
        scripted = assetDesc.script.nonEmpty,
        sponsored = assetDesc.sponsorship != 0
      )
    }
  }

  override def lastBlockOpt(): Option[BlockInfo] =
    blockchain.lastBlock.map(block => toBlockInfo(block.getHeader, height.toInt))

  override def blockInfoByHeight(blockHeight: Int): Option[BlockInfo] =
    blockchain.blockHeaderAndSize(blockHeight).map(blockHAndSize => toBlockInfo(blockHAndSize._1, blockHeight))

  private def toBlockInfo(blockH: BlockHeader, bHeight: Int) = {
    BlockInfo(
      timestamp = blockH.timestamp,
      height = bHeight,
      baseTarget = blockH.consensusData.baseTarget,
      generationSignature = blockH.consensusData.generationSignature,
      generator = blockH.signerData.generator.toAddress.bytes,
      generatorPublicKey = ByteStr(blockH.signerData.generator)
    )
  }
}
