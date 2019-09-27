package com.acrylplatform.account

import com.acrylplatform.common.utils.EitherExt2
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class AccountOrAliasTests extends PropSpec with PropertyChecks with Matchers {

  property("Account should get parsed correctly") {
    AddressOrAlias.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet() shouldBe an[Address]
    AddressOrAlias.fromString("address:3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet() shouldBe an[Address]

    Address.fromString("3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet() shouldBe an[Address]
    Address.fromString("address:3JGXFfC7P6oyvv3gXohbLoRzSvQWZeFBNNB").explicitGet() shouldBe an[Address]
  }

  property("Alias should get parsed correctly") {
    val alias = AddressOrAlias.fromString("alias:K:sasha").explicitGet().asInstanceOf[Alias]
    alias.name shouldBe "sasha"
    alias.chainId shouldBe 'K'

    val alias2 = Alias.fromString("alias:K:sasha").explicitGet()
    alias2.name shouldBe "sasha"
    alias2.chainId shouldBe 'K'

  }
  property("Alias cannot be from other network") {
    AddressOrAlias.fromString("alias:Q:sasha") shouldBe 'left
  }

  property("Malformed aliases cannot be reconstructed") {
    AddressOrAlias.fromString("alias::sasha") shouldBe 'left
    AddressOrAlias.fromString("alias:K: sasha") shouldBe 'left
    AddressOrAlias.fromString("alias:K:sasha\nivanov") shouldBe 'left
    AddressOrAlias.fromString("alias:K:s") shouldBe 'left
    AddressOrAlias.fromString("alias:KKK:sasha") shouldBe 'left

    Alias.fromString("alias:K: sasha") shouldBe 'left
    Alias.fromString("alias:K:sasha\nivanov") shouldBe 'left
    Alias.fromString("alias::sasha") shouldBe 'left
    Alias.fromString("alias:K:s") shouldBe 'left
    Alias.fromString("alias:KKK:sasha") shouldBe 'left

    Alias.fromString("aliaaas:A:sasha") shouldBe 'left
  }

  property("Unknown address schemes cannot be parsed") {
    AddressOrAlias.fromString("postcode:119072") shouldBe 'left
  }
}
