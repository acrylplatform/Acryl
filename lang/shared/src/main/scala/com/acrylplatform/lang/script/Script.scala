package com.acrylplatform.lang.script

import com.acrylplatform.common.state.ByteStr
import com.acrylplatform.common.utils.Base64
import com.acrylplatform.lang.ValidationError.ScriptParseError
import com.acrylplatform.lang.directives.values._
import com.acrylplatform.lang.script.ContractScript.ContractScriptImpl
import com.acrylplatform.lang.script.v1.ExprScript
import com.acrylplatform.lang.utils._
import com.acrylplatform.lang.v1.compiler.Decompiler
import monix.eval.Coeval

trait Script {
  type Expr

  val stdLibVersion: StdLibVersion

  val expr: Expr

  val bytes: Coeval[ByteStr]

  val complexityMap: Map[String, Long]
  val complexity: Long

  val containsBlockV2: Coeval[Boolean]

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Script => stdLibVersion == that.stdLibVersion && expr == that.expr
    case _            => false
  }

  override def hashCode(): Int = stdLibVersion.id * 31 + expr.hashCode()
}

object Script {

  val checksumLength = 4

  def fromBase64String(str: String, checkComplexity: Boolean = true): Either[ScriptParseError, Script] =
    for {
      bytes  <- Base64.tryDecode(str).toEither.left.map(ex => ScriptParseError(s"Unable to decode base64: ${ex.getMessage}"))
      script <- ScriptReader.fromBytes(bytes, checkComplexity)
    } yield script

  type DirectiveMeta = List[(String, Any)]

  def decompile(s: Script): (String, DirectiveMeta) = {
    val ctx = defaultDecompilerContext
    val (scriptText, directives) = s match {
      case e: ExprScript                      => (Decompiler(e.expr, ctx), List(s.stdLibVersion, Expression))
      case ContractScriptImpl(_, contract, _) => (Decompiler(contract, ctx), List(s.stdLibVersion, Account, DApp))
    }
    val directivesText = directives
      .map(_.unparsed)
      .mkString(start = "", sep = "\n", end = "\n")

    val meta = directives.map(d => (d.key.text, d.value))
    (directivesText + scriptText, meta)
  }
}
