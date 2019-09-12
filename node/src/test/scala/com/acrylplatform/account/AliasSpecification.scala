package com.acrylplatform.account

import com.acrylplatform.{NoShrink, TransactionGen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class AliasSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("Correct alias should be valid") {
    forAll(validAliasStringGen) { s =>
      Alias.create(s) shouldBe 'right
    }
  }

  property("Incorrect alias should be invalid") {
    forAll(invalidAliasStringGen) { s =>
      Alias.create(s) shouldBe 'left
    }
  }
}
