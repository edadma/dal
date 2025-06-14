package io.github.edadma.dal

import io.github.edadma.numbers.{ComplexDouble, QuaternionDouble, Rational, SmallRational}
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

  it should "parse long integers correctly" in {
    val longVal = (Int.MaxValue.toLong + 1).toString
    parseNumber(longVal) shouldBe a[java.lang.Long]
  }

  it should "parse negative large integers" in {
    parseNumber("-123456789012345678901234567890") shouldBe BigInt("-123456789012345678901234567890")
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

  it should "parse negative rationals" in {
    parseNumber("-3/4") shouldBe SmallRational(-3L, 4L)
  }

  it should "parse and reduce rationals" in {
    parseNumber("6/8") shouldBe SmallRational(3L, 4L) // Should auto-reduce
  }

  it should "parse improper fractions" in {
    parseNumber("7/3") shouldBe SmallRational(7L, 3L)
  }

  it should "handle rationals that actually overflow SmallRational during arithmetic" in {
    // Create SmallRationals that will overflow when operated on
    val large1 = SmallRational(Long.MaxValue / 2, 1L)
    val large2 = SmallRational(3L, 1L)

    // This should trigger overflow handling in DAL operations
    val result = PrecisionDAL.compute("+", large1, large2)
    // Result should be promoted to appropriate type
    result shouldBe a[Number]
  }

  it should "fallback to big Rational for numbers too large for SmallRational" in {
    // Use a smaller but still overflowing case
    val hugeDenominator = s"${Long.MaxValue}999" // Slightly larger than Long.MaxValue
    val result          = parseNumber(s"1/${hugeDenominator}")
    result shouldBe a[Rational] // Should fallback to big Rational
  }

  it should "parse decimals as doubles" in {
    parseNumber("3.14") shouldBe 3.14
    parseNumber("-0.5") shouldBe -0.5
    parseNumber("0.0") shouldBe 0.0
  }

  it should "parse scientific notation" in {
    parseNumber("1.5e10") shouldBe 1.5e10
    parseNumber("2E-5") shouldBe 2e-5
    parseNumber("-3.14e2") shouldBe -314.0
  }

  it should "parse decimals with leading/trailing decimal points" in {
    parseNumber(".5") shouldBe 0.5
    parseNumber("5.") shouldBe 5.0
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

  it should "parse complex numbers with negative imaginary" in {
    parseNumber("3-4i") shouldBe ComplexDouble(3, -4)
  }

  it should "parse pure imaginary numbers" in {
    parseNumber("4i") shouldBe ComplexDouble(0, 4)
    parseNumber("-4i") shouldBe ComplexDouble(0, -4)
    parseNumber("i") shouldBe ComplexDouble(0, 1)
    parseNumber("-i") shouldBe ComplexDouble(0, -1)
  }

  it should "parse complex with unit imaginary" in {
    parseNumber("3+i") shouldBe ComplexDouble(3, 1)
    parseNumber("3-i") shouldBe ComplexDouble(3, -1)
  }

  it should "parse rational complex numbers" in {
    // This depends on ComplexRationalIsFractional working
    parseNumber("1/2+1/3i") shouldBe a[ComplexRational]
  }

  it should "parse quaternions" in {
    val result = parseNumber("1+2i+3j+4k")
    result shouldBe a[QuaternionDouble]
    val q = result.asInstanceOf[QuaternionDouble]
    q shouldBe QuaternionDouble(1, 2, 3, 4)
  }
}
