package com.acrylplatform.lang

import cats.kernel.Monoid
import com.acrylplatform.lang.directives.values.V2
import com.acrylplatform.lang.v1.compiler.ExpressionCompiler
import com.acrylplatform.lang.v1.compiler.Terms.EXPR
import com.acrylplatform.lang.v1.evaluator.ctx.impl.acryl.AcrylContext
import com.acrylplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}

object JavaAdapter {
  private val version = V2

  lazy val ctx =
    Monoid.combineAll(
      Seq(
        CryptoContext.compilerContext(Global, version),
        AcrylContext.build(???, null).compilerContext,
        PureContext.build(Global, version).compilerContext
      ))

  def compile(input: String): EXPR = {
    ExpressionCompiler
      .compile(input, ctx)
      .fold(
        error => throw new IllegalArgumentException(error),
        expr => expr
      )
  }
}
