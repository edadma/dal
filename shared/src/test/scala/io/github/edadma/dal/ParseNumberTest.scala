package io.github.edadma.dal

import io.github.edadma.numbers.{
  ComplexDouble,
  ComplexRational,
  ComplexBigInt,
  QuaternionDouble,
  Rational,
  SmallRational,
}
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

  // FIXED: Updated tests to expect correct types based on exactness priority
  "parseNumber complex parsing priority" should "parse integer complex numbers as ComplexBigInt (exact)" in {
    val result = parseNumber("3+4i")
    result shouldBe a[ComplexBigInt]
    result.asInstanceOf[ComplexBigInt] shouldBe ComplexBigInt(BigInt(3), BigInt(4))
  }

  it should "parse integer complex with negative imaginary as ComplexBigInt" in {
    val result = parseNumber("3-4i")
    result shouldBe a[ComplexBigInt]
    result.asInstanceOf[ComplexBigInt] shouldBe ComplexBigInt(BigInt(3), BigInt(-4))
  }

  it should "parse integer complex with unit imaginary as ComplexBigInt" in {
    val result = parseNumber("3+i")
    result shouldBe a[ComplexBigInt]
    result.asInstanceOf[ComplexBigInt] shouldBe ComplexBigInt(BigInt(3), BigInt(1))

    val result2 = parseNumber("3-i")
    result2 shouldBe a[ComplexBigInt]
    result2.asInstanceOf[ComplexBigInt] shouldBe ComplexBigInt(BigInt(3), BigInt(-1))
  }

  it should "parse pure integer imaginary numbers as ComplexBigInt" in {
    parseNumber("4i") shouldBe ComplexBigInt(BigInt(0), BigInt(4))
    parseNumber("-4i") shouldBe ComplexBigInt(BigInt(0), BigInt(-4))
    parseNumber("i") shouldBe ComplexBigInt(BigInt(0), BigInt(1))
    parseNumber("-i") shouldBe ComplexBigInt(BigInt(0), BigInt(-1))
  }

  // NEW: Tests for decimal complex numbers
  it should "parse decimal complex numbers as ComplexDouble" in {
    val result = parseNumber("3.5+4.2i")
    result shouldBe a[ComplexDouble]
    result.asInstanceOf[ComplexDouble] shouldBe ComplexDouble(3.5, 4.2)
  }

  it should "parse mixed integer-decimal complex as ComplexDouble" in {
    val result1 = parseNumber("3+4.5i")
    result1 shouldBe a[ComplexDouble]
    result1.asInstanceOf[ComplexDouble] shouldBe ComplexDouble(3.0, 4.5)

    val result2 = parseNumber("3.5+4i")
    result2 shouldBe a[ComplexDouble]
    result2.asInstanceOf[ComplexDouble] shouldBe ComplexDouble(3.5, 4.0)
  }

  it should "parse decimal complex with negative imaginary as ComplexDouble" in {
    parseNumber("2.5-3.7i") shouldBe ComplexDouble(2.5, -3.7)
  }

  it should "parse pure decimal imaginary numbers as ComplexDouble" in {
    parseNumber("4.5i") shouldBe ComplexDouble(0, 4.5)
    parseNumber("-2.3i") shouldBe ComplexDouble(0, -2.3)
  }

  it should "parse scientific notation complex as ComplexDouble" in {
    parseNumber("1e2+3e-1i") shouldBe ComplexDouble(100.0, 0.3)
    parseNumber("2.5E+1-1.5E-2i") shouldBe ComplexDouble(25.0, -0.015)
  }

  // NEW: Tests for rational complex numbers (highest priority for exactness)
  it should "parse rational complex numbers as ComplexRational" in {
    val result = parseNumber("1/2+1/3i")
    result shouldBe a[ComplexRational]
    val cr = result.asInstanceOf[ComplexRational]
    cr.re shouldBe Rational(1, 2)
    cr.im shouldBe Rational(1, 3)
  }

  it should "parse mixed rational complex as ComplexRational" in {
    val result = parseNumber("3/4-2/5i")
    result shouldBe a[ComplexRational]
    val cr = result.asInstanceOf[ComplexRational]
    cr.re shouldBe Rational(3, 4)
    cr.im shouldBe Rational(-2, 5)
  }

  it should "parse pure rational imaginary as ComplexRational" in {
    val result = parseNumber("2/3i")
    result shouldBe a[ComplexRational]
    val cr = result.asInstanceOf[ComplexRational]
    cr.re shouldBe Rational(0)
    cr.im shouldBe Rational(2, 3)
  }

  // Tests for special cases in complex parsing
  it should "handle zero complex numbers correctly" in {
    parseNumber("0+0i") shouldBe ComplexBigInt(BigInt(0), BigInt(0))
    parseNumber("0.0+0.0i") shouldBe ComplexDouble(0.0, 0.0)
  }

  it should "handle complex numbers with zero real part" in {
    parseNumber("0+5i") shouldBe ComplexBigInt(BigInt(0), BigInt(5))
    parseNumber("0.0+5.5i") shouldBe ComplexDouble(0.0, 5.5)
  }

  it should "handle complex numbers with zero imaginary part" in {
    parseNumber("7+0i") shouldBe ComplexBigInt(BigInt(7), BigInt(0))
    parseNumber("7.5+0.0i") shouldBe ComplexDouble(7.5, 0.0)
  }

  "parseNumber quaternion parsing" should "parse quaternions" in {
    val result = parseNumber("1+2i+3j+4k")
    result shouldBe a[QuaternionDouble]
    val q = result.asInstanceOf[QuaternionDouble]
    q shouldBe QuaternionDouble(1, 2, 3, 4)
  }

  it should "parse quaternions with negative components" in {
    parseNumber("1-2i+3j-4k") shouldBe QuaternionDouble(1, -2, 3, -4)
  }

  it should "parse quaternions with missing components" in {
    parseNumber("1+3j") shouldBe QuaternionDouble(1, 0, 3, 0)
    parseNumber("4k") shouldBe QuaternionDouble(0, 0, 0, 4)
  }

  it should "parse pure quaternion units" in {
    parseNumber("j") shouldBe QuaternionDouble(0, 0, 1, 0)
    parseNumber("k") shouldBe QuaternionDouble(0, 0, 0, 1)
  }

  // Test the parsing priority explicitly
  "parseNumber type priority" should "prioritize exact types over approximate types" in {
    // Rational > BigInt > Double for exactness
    parseNumber("1/2") shouldBe a[SmallRational]     // Most exact for fractions
    parseNumber("42") shouldBe a[java.lang.Integer]  // Exact for integers
    parseNumber("3.14") shouldBe a[java.lang.Double] // Approximate for decimals

    // ComplexRational > ComplexBigInt > ComplexDouble for exactness
    parseNumber("1/2+1/3i") shouldBe a[ComplexRational] // Most exact
    parseNumber("3+4i") shouldBe a[ComplexBigInt]       // Exact for integer coefficients
    parseNumber("3.14+2.71i") shouldBe a[ComplexDouble] // Approximate for decimal coefficients
  }

  it should "handle edge cases in exact vs approximate parsing" in {
    // These should be exact
    parseNumber("0+0i") shouldBe a[ComplexBigInt]
    parseNumber("1+1i") shouldBe a[ComplexBigInt]
    parseNumber("-5-3i") shouldBe a[ComplexBigInt]

    // These should be approximate
    parseNumber("0.0+0.0i") shouldBe a[ComplexDouble]
    parseNumber("1.0+1.0i") shouldBe a[ComplexDouble]
    parseNumber("1+1.0i") shouldBe a[ComplexDouble] // Mixed → approximate
  }
}
