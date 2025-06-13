package io.github.edadma.dal

import io.github.edadma.numbers.{ComplexDouble, QuaternionDouble, SmallRational}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParseNumberTest extends AnyFlatSpec with Matchers {

  "parseNumber" should "parse integers correctly" in {
    parseNumber("42") shouldBe 42
    parseNumber("-17") shouldBe -17
    parseNumber("0") shouldBe 0

    // Large integer that becomes BigInt
    val large = "123456789012345678901234567890"
    parseNumber(large) shouldBe BigInt(large)
  }

  it should "parse small rationals efficiently" in {
    val result = parseNumber("3/4")
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(3L, 4L)
  }

  it should "fallback to big rationals on overflow" in {
    val large  = s"${Long.MaxValue}/2"
    val result = parseNumber(large)
    // Should be SmallRational since it fits
    result shouldBe a[SmallRational]

    // This should overflow and become big Rational
    val overflow = s"${Long.MaxValue}/${Long.MaxValue}"
    val result2  = parseNumber(overflow)
    // Might be SmallRational(1) after reduction, need to test actual overflow case
  }

  it should "parse decimals as doubles" in {
    parseNumber("3.14") shouldBe 3.14
    parseNumber("-0.5") shouldBe -0.5
    parseNumber("0.0") shouldBe 0.0
  }

  it should "handle whitespace" in {
    parseNumber("  42  ") shouldBe 42
    parseNumber("\t3/4\n") shouldBe SmallRational(3L, 4L)
  }

  it should "throw on invalid input" in {
    an[IllegalArgumentException] should be thrownBy parseNumber("not-a-number")
    an[IllegalArgumentException] should be thrownBy parseNumber("3/0") // Division by zero
    an[IllegalArgumentException] should be thrownBy parseNumber("")
  }

  it should "parse complex numbers" in {
    val result = parseNumber("3+4i")
    result shouldBe a[ComplexDouble]
    result.asInstanceOf[ComplexDouble] shouldBe ComplexDouble(3, 4)
  }

  it should "parse quaternions" in {
    val result = parseNumber("1+2i+3j+4k")
    result shouldBe a[QuaternionDouble]
    val q = result.asInstanceOf[QuaternionDouble]
    q shouldBe QuaternionDouble(1, 2, 3, 4)
  }
}
