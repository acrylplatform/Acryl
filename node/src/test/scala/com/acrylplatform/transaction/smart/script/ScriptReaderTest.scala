package com.acrylplatform.transaction.smart.script

import com.acrylplatform.common.utils._
import com.acrylplatform.lang.directives.DirectiveDictionary
import com.acrylplatform.lang.directives.values._
import com.acrylplatform.lang.script.{ContractScript, ScriptReader}
import com.acrylplatform.lang.v1.Serde
import com.acrylplatform.lang.v1.testing.TypedScriptGen
import com.acrylplatform.state.diffs.produce
import com.acrylplatform.{NoShrink, crypto}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import org.scalatest.{Inside, Matchers, PropSpec}

class ScriptReaderTest extends PropSpec with PropertyChecks with Matchers with TypedScriptGen with Inside with NoShrink {
  val checksumLength = 4

  property("should parse all bytes for V1") {
    forAll(exprGen) { sc =>
      val body     = Array(V1.id.toByte) ++ Serde.serialize(sc) ++ "foo".getBytes("UTF-8")
      val allBytes = body ++ crypto.secureHash(body).take(checksumLength)
      ScriptReader.fromBytes(allBytes) should produce("bytes left")
    }
  }

  property("should parse all bytes for V3") {
    forAll(contractGen) { sc =>
      val allBytes = ContractScript.apply(V3, sc).explicitGet().bytes().arr
      ScriptReader.fromBytes(allBytes).explicitGet().expr shouldBe sc
    }
  }

  property("should parse expression with all supported std lib version") {
    val scriptEthList =
      DirectiveDictionary[StdLibVersion].all.map { version =>
        ScriptCompiler.compile(s"""
                                  |{-# STDLIB_VERSION ${version.value} #-}
                                  |  true
                                  """.stripMargin)
      }
    scriptEthList.foreach(_ shouldBe 'right)

    scriptEthList.foreach { scriptEth =>
      ScriptReader.fromBytes(scriptEth.explicitGet()._1.bytes()) shouldBe 'right
    }
  }
}
