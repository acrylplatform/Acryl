package com.acrylplatform.api.grpc

import com.acrylplatform.api.common.CommonTransactionsApi
import com.acrylplatform.protobuf.transaction.{InvokeScriptResult, PBSignedTransaction, VanillaTransaction}
import com.acrylplatform.state.{Blockchain, TransactionId}
import com.acrylplatform.transaction.AuthorizedTransaction
import com.acrylplatform.transaction.transfer.TransferTransaction
import com.acrylplatform.utx.UtxPool
import com.acrylplatform.wallet.Wallet
import io.grpc.stub.StreamObserver
import io.grpc.{Status, StatusRuntimeException}
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.Future

class TransactionsApiGrpcImpl(wallet: Wallet,
                              blockchain: Blockchain,
                              utx: UtxPool,
                              broadcast: VanillaTransaction => Unit)(implicit sc: Scheduler)
    extends TransactionsApiGrpc.TransactionsApi {

  private[this] val commonApi = new CommonTransactionsApi(blockchain, utx, wallet, (tx, _) => broadcast(tx))

  override def getTransactions(request: TransactionsRequest, responseObserver: StreamObserver[TransactionResponse]): Unit = {
    val stream = commonApi
      .transactionsByAddress(request.sender.toAddress)
      .map {
        case (height, transaction) if transactionFilter(request, transaction) => TransactionResponse(transaction.id(), height, Some(transaction.toPB))
      }

    responseObserver.completeWith(stream)
  }

  override def getUnconfirmed(request: TransactionsRequest, responseObserver: StreamObserver[TransactionResponse]): Unit = {
    val stream = Observable(commonApi.unconfirmedTransactions(): _*)
      .filter(transactionFilter(request, _))
      .map(tx => TransactionResponse(tx.id(), transaction = Some(tx.toPB)))

    responseObserver.completeWith(stream)
  }

  override def getStateChanges(request: TransactionsRequest, responseObserver: StreamObserver[InvokeScriptResult]): Unit = {
    import com.acrylplatform.state.{InvokeScriptResult => VISR}

    val result = Observable(request.transactionIds: _*)
      .flatMap(txId => Observable.fromIterable(blockchain.invokeScriptResult(TransactionId(txId.toByteStr)).toOption))
      .map(VISR.toPB)

    responseObserver.completeWith(result)
  }

  override def getStatuses(request: TransactionsByIdRequest, responseObserver: StreamObserver[TransactionStatus]): Unit = {
    val result = Observable(request.transactionIds: _*).map { txId =>
      blockchain.transactionHeight(txId) match {
        case Some(height) => TransactionStatus(txId, TransactionStatus.Status.CONFIRMED, height)

        case None =>
          utx.transactionById(txId) match {
            case Some(_) => TransactionStatus(txId, TransactionStatus.Status.UNCONFIRMED)
            case None    => TransactionStatus(txId, TransactionStatus.Status.NOT_EXISTS)
          }
      }
    }
    responseObserver.completeWith(result)
  }

  override def sign(request: SignRequest): Future[PBSignedTransaction] =
    throw new StatusRuntimeException(Status.UNIMPLEMENTED)

  override def broadcast(tx: PBSignedTransaction): Future[PBSignedTransaction] = {
    commonApi
      .broadcastTransaction(tx.toVanilla)
      .map(_.toPB)
      .resultE
      .toFuture
  }

  private[this] def transactionFilter(request: TransactionsRequest, tx: VanillaTransaction): Boolean = {
    val senderMatches = request.sender.isEmpty || (tx match {
      case a: AuthorizedTransaction => request.sender.isEmpty || a.sender.toAddress == request.sender.toAddress
      case _                        => false
    })

    val recipientMatches = tx match {
      case tt: TransferTransaction => request.recipient.isEmpty || tt.recipient == request.getRecipient.toAddressOrAlias
      case _                       => request.recipient.isEmpty
    }

    val transactionIdMatches = request.transactionIds.isEmpty || request.transactionIds.contains(tx.id().toPBByteString)
    senderMatches && recipientMatches && transactionIdMatches
  }
}
