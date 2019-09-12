package com.acrylplatform.transaction.smart

import cats.kernel.Monoid
import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.lang.directives.DirectiveSet
import com.acrylplatform.lang.directives.values.{ContentType, ScriptType, StdLibVersion}
import com.acrylplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.acrylplatform.lang.v1.evaluator.ctx.impl.acryl.AcrylContext
import com.acrylplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.acrylplatform.lang.{ExecutionError, Global}
import com.acrylplatform.state._
import monix.eval.Coeval

object BlockchainContext {

  type In = AcrylEnvironment.In
  def build(version: StdLibVersion,
            nByte: Byte,
            in: Coeval[In],
            h: Coeval[Int],
            blockchain: Blockchain,
            isTokenContext: Boolean,
            isContract: Boolean,
            address: Coeval[ByteStr]): Either[ExecutionError, EvaluationContext] =
    DirectiveSet(
      version,
      ScriptType.isAssetScript(isTokenContext),
      ContentType.isDApp(isContract)
    ).map(AcrylContext.build(_, new AcrylEnvironment(nByte, in, h, blockchain, address)))
      .map(Seq(PureContext.build(Global, version), CryptoContext.build(Global, version), _))
      .map(Monoid.combineAll(_))
      .map(_.evaluationContext)
}
