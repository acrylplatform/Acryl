package com.acrylplatform.history

import com.acrylplatform.db.WithDomain
import com.acrylplatform.settings.AcrylSettings
import org.scalacheck.Gen
import org.scalatest.{Assertion, Suite}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks => GeneratorDrivenPropertyChecks}

trait DomainScenarioDrivenPropertyCheck extends WithDomain { _: Suite with GeneratorDrivenPropertyChecks =>
  def scenario[S](gen: Gen[S], bs: AcrylSettings = DefaultAcrylSettings)(assertion: (Domain, S) => Assertion): Assertion =
    forAll(gen) { s =>
      withDomain(bs) { domain =>
        assertion(domain, s)
      }
    }
}
