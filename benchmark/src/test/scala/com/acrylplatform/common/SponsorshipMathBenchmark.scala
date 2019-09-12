package com.acrylplatform.common
import java.util.concurrent.TimeUnit

import com.acrylplatform.state.diffs.CommonValidation
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@Threads(4)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class SponsorshipMathBenchmark {
  @Benchmark
  def bigDecimal_test(bh: Blackhole): Unit = {
    def toAcryl(assetFee: Long, sponsorship: Long): Long = {
      val acryl = (BigDecimal(assetFee) * BigDecimal(CommonValidation.FeeUnit)) / BigDecimal(sponsorship)
      if (acryl > Long.MaxValue) {
        throw new java.lang.ArithmeticException("Overflow")
      }
      acryl.toLong
    }

    bh.consume(toAcryl(100000, 100000000))
  }

  @Benchmark
  def bigInt_test(bh: Blackhole): Unit = {
    def toAcryl(assetFee: Long, sponsorship: Long): Long = {
      val acryl = BigInt(assetFee) * CommonValidation.FeeUnit / sponsorship
      acryl.bigInteger.longValueExact()
    }

    bh.consume(toAcryl(100000, 100000000))
  }
}
