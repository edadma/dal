package io.github.edadma.dal

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.github.edadma.numbers._

class ComplexDALTest extends AnyFlatSpec with Matchers {

  "ComplexDAL basic arithmetic" should "handle real number operations like PrecisionDAL" in {
    // Integer arithmetic
    eval("3 + 4", ComplexDAL) shouldBe 7
    eval("10 - 3", ComplexDAL) shouldBe 7
    eval("6 * 7", ComplexDAL) shouldBe 42

    // Should create exact rationals for division
    val result = eval("1 / 3", ComplexDAL)
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(1L, 3L)
  }

  it should "handle mixed integer and decimal operations" in {
    eval("3 + 4.5", ComplexDAL) shouldBe 7.5
    eval("10.0 - 3", ComplexDAL) shouldBe 7.0
    eval("2.5 * 4", ComplexDAL) shouldBe 10.0
  }

  it should "handle rational number arithmetic" in {
    val result1 = eval("1/2 + 1/3", ComplexDAL)
    result1 shouldBe a[SmallRational]
    result1.asInstanceOf[SmallRational] shouldBe SmallRational(5L, 6L)

    val result2 = eval("3/4 * 2/5", ComplexDAL)
    result2 shouldBe a[SmallRational]
    result2.asInstanceOf[SmallRational] shouldBe SmallRational(3L, 10L)
  }

  "ComplexDAL modulo operations" should "handle integer modulo" in {
    eval("10 mod 3", ComplexDAL) shouldBe 1
    eval("17 mod 5", ComplexDAL) shouldBe 2
    eval("8 mod 4", ComplexDAL) shouldBe 0
  }

  it should "handle decimal modulo" in {
    val result = eval("5.5 mod 2.2", ComplexDAL)
    result shouldBe a[java.lang.Double]
    result.asInstanceOf[Double] shouldBe 1.1 +- 0.001
  }

  "ComplexDAL integer division" should "handle integer division" in {
    eval("7 \\ 2", ComplexDAL) shouldBe 3
    eval("15 \\ 4", ComplexDAL) shouldBe 3
    eval("20 \\ 5", ComplexDAL) shouldBe 4
  }

  it should "handle rational integer division" in {
    val result1 = eval("7/2 \\ 3/2", ComplexDAL)
    result1 shouldBe a[java.lang.Integer]
    result1.asInstanceOf[Int] shouldBe 2 // (7/2) / (3/2) = 7/3 = 2.33... → 2

    val result2 = eval("9/3 \\ 1", ComplexDAL)
    result2 shouldBe a[java.lang.Integer]
    result2.asInstanceOf[Int] shouldBe 3 // 9/3 = 3, 3/1 = 3
  }

  "ComplexDAL complex arithmetic" should "handle basic complex number operations" in {
    // Complex + Complex
    val result1 = eval("3+4i + 1+2i", ComplexDAL)
    result1 shouldBe a[ComplexDouble]
    result1.asInstanceOf[ComplexDouble] shouldBe ComplexDouble(4, 6)

    // Complex - Complex
    val result2 = eval("5+3i - 2+1i", ComplexDAL)
    result2 shouldBe a[ComplexDouble]
    result2.asInstanceOf[ComplexDouble] shouldBe ComplexDouble(3, 2)

    // Complex * Complex
    val result3 = eval("2+3i * 1+1i", ComplexDAL)
    result3 shouldBe a[ComplexDouble]
    // (2+3i)(1+1i) = 2 + 2i + 3i + 3i² = 2 + 5i - 3 = -1 + 5i
    result3.asInstanceOf[ComplexDouble] shouldBe ComplexDouble(-1, 5)
  }

  it should "handle complex division" in {
    val result = eval("4+2i / 2+0i", ComplexDAL)
    result shouldBe a[ComplexDouble]
    result.asInstanceOf[ComplexDouble] shouldBe ComplexDouble(2, 1)
  }

  it should "handle complex rational arithmetic" in {
    // This tests exact complex rational arithmetic
    val result = eval("1/2+1/3i + 1/4+1/6i", ComplexDAL)
    result shouldBe a[ComplexRational]
    val cr = result.asInstanceOf[ComplexRational]
    cr.re shouldBe Rational(3, 4) // 1/2 + 1/4 = 2/4 + 1/4 = 3/4
    cr.im shouldBe Rational(1, 2) // 1/3 + 1/6 = 2/6 + 1/6 = 3/6 = 1/2
  }

  "ComplexDAL all comparison operations" should "handle equality and inequality" in {
    eval("3 = 3", ComplexDAL) shouldBe true
    eval("3 = 4", ComplexDAL) shouldBe false
    eval("3 != 4", ComplexDAL) shouldBe true
    eval("3 != 3", ComplexDAL) shouldBe false
    eval("1/2 = 0.5", ComplexDAL) shouldBe true
  }

  it should "handle ordering comparisons" in {
    eval("5 > 3", ComplexDAL) shouldBe true
    eval("2 < 7", ComplexDAL) shouldBe true
    eval("5 >= 5", ComplexDAL) shouldBe true
    eval("5 >= 3", ComplexDAL) shouldBe true
    eval("3 <= 5", ComplexDAL) shouldBe true
    eval("5 <= 5", ComplexDAL) shouldBe true
    eval("7 <= 3", ComplexDAL) shouldBe false
  }

  it should "handle divisibility tests" in {
    eval("3 div 6", ComplexDAL) shouldBe true   // 3 divides 6: 6/3 = 2 (whole number)
    eval("3 div 7", ComplexDAL) shouldBe false  // 3 divides 7: 7/3 = 2.33... (not whole)
    eval("4 div 12", ComplexDAL) shouldBe true  // 4 divides 12: 12/4 = 3 (whole number)
    eval("2 div 15", ComplexDAL) shouldBe false // 2 divides 15: 15/2 = 7.5 (not whole)
  }

  it should "handle rational divisibility" in {
    eval("1/2 div 3/2", ComplexDAL) shouldBe true  // (1/2) divides (3/2): (3/2) / (1/2) = 3 (whole number)
    eval("1/3 div 5/3", ComplexDAL) shouldBe true  // (1/3) divides (5/3): (5/3) / (1/3) = 5 (whole number)
    eval("1/2 div 2/3", ComplexDAL) shouldBe false // (1/2) divides (2/3): (2/3) / (1/2) = 4/3 (not whole)
  }

  it should "handle complex number equality" in {
    eval("3+4i = 3+4i", ComplexDAL) shouldBe true
    eval("3+4i = 3+5i", ComplexDAL) shouldBe false
    eval("2+0i = 2", ComplexDAL) shouldBe true // Real number should equal complex with 0 imaginary
    eval("3+4i != 3+5i", ComplexDAL) shouldBe true
  }

  "ComplexDAL bitwise operations" should "handle integer bitwise operations" in {
    eval("5 and 3", ComplexDAL) shouldBe 1 // 101 & 011 = 001
    eval("5 or 3", ComplexDAL) shouldBe 7  // 101 | 011 = 111
    eval("5 xor 3", ComplexDAL) shouldBe 6 // 101 ^ 011 = 110
  }

  it should "handle long bitwise operations" in {
    eval("12345678 and 87654321", ComplexDAL) shouldBe (12345678 & 87654321)
    eval("12345678 or 87654321", ComplexDAL) shouldBe (12345678 | 87654321)
    eval("12345678 xor 87654321", ComplexDAL) shouldBe (12345678 ^ 87654321)
  }

  "ComplexDAL power operations" should "handle real number powers" in {
    eval("2 ^ 3", ComplexDAL) shouldBe BigInt(8)
    eval("3 ^ 2", ComplexDAL) shouldBe BigInt(9)

    val result = eval("1/2 ^ 2", ComplexDAL)
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(1L, 4L)
  }

  it should "handle complex number powers with integer exponents" in {
    // i^2 = -1
    val result1 = eval("0+1i ^ 2", ComplexDAL)
    result1 shouldBe a[ComplexDouble]
    val complex1 = result1.asInstanceOf[ComplexDouble]
    complex1.re shouldBe -1.0 +- 0.001
    complex1.im shouldBe 0.0 +- 0.001

    // (1+i)^2 = 1 + 2i + i^2 = 1 + 2i - 1 = 2i
    val result2 = eval("1+1i ^ 2", ComplexDAL)
    result2 shouldBe a[ComplexDouble]
    val complex2 = result2.asInstanceOf[ComplexDouble]
    complex2.re shouldBe 0.0 +- 0.001
    complex2.im shouldBe 2.0 +- 0.001
  }

  it should "handle complex powers with complex exponents" in {
    val result = eval("2+0i ^ 1+1i", ComplexDAL)
    result shouldBe a[ComplexDouble]
    // This should compute 2^(1+i) using complex exponentiation
    result.asInstanceOf[ComplexDouble].re should not be 0.0
  }

  "ComplexDAL special type promotions" should "promote Double + ComplexBigInt to ComplexDouble" in {
    // Creating a ComplexBigInt programmatically since parsing doesn't support it directly
    val complexBigInt = ComplexBigInt(BigInt(123), BigInt(456))
    val result        = ComplexDAL.compute("+", 2.5, complexBigInt)
    result shouldBe a[ComplexDouble]
    val cd = result.asInstanceOf[ComplexDouble]
    cd.re shouldBe 125.5 +- 0.001
    cd.im shouldBe 456.0 +- 0.001
  }

  it should "promote BigDecimal + ComplexBigInt to ComplexBigDecimal" in {
    val complexBigInt = ComplexBigInt(BigInt(789), BigInt(321))
    val result        = ComplexDAL.compute("+", BigDecimal("3.14159"), complexBigInt)
    result shouldBe a[ComplexBigDecimal]
    val cbd = result.asInstanceOf[ComplexBigDecimal]
    cbd.re shouldBe BigDecimal("792.14159") +- BigDecimal("0.00001")
    cbd.im shouldBe BigDecimal(321) +- BigDecimal("0.00001")
  }

  it should "promote Rational + ComplexBigInt to ComplexRational" in {
    val complexBigInt = ComplexBigInt(BigInt(5), BigInt(7))
    val result        = ComplexDAL.compute("+", Rational(1, 3), complexBigInt)
    result shouldBe a[ComplexRational]
    val cr = result.asInstanceOf[ComplexRational]
    cr.re shouldBe Rational(16, 3) // 5 + 1/3 = 15/3 + 1/3 = 16/3
    cr.im shouldBe Rational(7, 1)
  }

  it should "promote SmallRational + ComplexBigInt to ComplexRational" in {
    val complexBigInt = ComplexBigInt(BigInt(3), BigInt(4))
    val result        = ComplexDAL.compute("+", SmallRational(1L, 2L), complexBigInt)
    result shouldBe a[ComplexRational]
    val cr = result.asInstanceOf[ComplexRational]
    cr.re shouldBe Rational(7, 2) // 3 + 1/2 = 6/2 + 1/2 = 7/2
    cr.im shouldBe Rational(4, 1)
  }

  it should "promote Double + ComplexRational to ComplexDouble" in {
    val complexRational = ComplexRational(Rational(1, 2), Rational(1, 3))
    val result          = ComplexDAL.compute("*", 2.0, complexRational)
    result shouldBe a[ComplexDouble]
    val cd = result.asInstanceOf[ComplexDouble]
    cd.re shouldBe 1.0 +- 0.001         // 2 * (1/2) = 1
    cd.im shouldBe (2.0 / 3.0) +- 0.001 // 2 * (1/3) = 2/3
  }

  it should "promote BigDecimal + ComplexRational to ComplexBigDecimal" in {
    val complexRational = ComplexRational(Rational(3, 4), Rational(5, 6))
    val result          = ComplexDAL.compute("+", BigDecimal("1.25"), complexRational)
    result shouldBe a[ComplexBigDecimal]
    val cbd = result.asInstanceOf[ComplexBigDecimal]
    cbd.re shouldBe BigDecimal("2") +- BigDecimal("0.001")        // 1.25 + 0.75 = 2
    cbd.im shouldBe BigDecimal("0.833333") +- BigDecimal("0.001") // 5/6 ≈ 0.833333
  }

  it should "promote BigDecimal + ComplexDouble to ComplexBigDecimal" in {
    val complexDouble = ComplexDouble(2.718, 3.14159)
    val result        = ComplexDAL.compute("-", BigDecimal("10.5"), complexDouble)
    result shouldBe a[ComplexBigDecimal]
    val cbd = result.asInstanceOf[ComplexBigDecimal]
    cbd.re shouldBe BigDecimal("7.782") +- BigDecimal("0.001")    // 10.5 - 2.718
    cbd.im shouldBe BigDecimal("-3.14159") +- BigDecimal("0.001") // 0 - 3.14159
  }

  "ComplexDAL mixed operations" should "handle real + complex" in {
    val result1 = eval("3 + 2+4i", ComplexDAL)
    result1 shouldBe a[ComplexDouble]
    result1.asInstanceOf[ComplexDouble] shouldBe ComplexDouble(5, 4)

    val result2 = eval("2+4i + 3", ComplexDAL)
    result2 shouldBe a[ComplexDouble]
    result2.asInstanceOf[ComplexDouble] shouldBe ComplexDouble(5, 4)
  }

  it should "handle rational + complex" in {
    val result = eval("1/2 + 1+1i", ComplexDAL)
    result shouldBe a[ComplexDouble] // Promotes to ComplexDouble
    val complex = result.asInstanceOf[ComplexDouble]
    complex.re shouldBe 1.5 +- 0.001
    complex.im shouldBe 1.0 +- 0.001
  }

  "ComplexDAL float division" should "provide floating point division when requested" in {
    eval("1 // 3", ComplexDAL) shouldBe (1.0 / 3.0)
    eval("7 // 2", ComplexDAL) shouldBe 3.5

    // Complex float division
    val result = eval("1+1i // 2+0i", ComplexDAL)
    result shouldBe a[ComplexDouble]
    result.asInstanceOf[ComplexDouble] shouldBe ComplexDouble(0.5, 0.5)
  }

  it should "handle rational float division" in {
    val result = eval("3/4 // 1/2", ComplexDAL)
    result shouldBe a[java.lang.Double]
    result.asInstanceOf[Double] shouldBe 1.5 +- 0.001 // (3/4) / (1/2) = 1.5
  }

  "ComplexDAL three-way comparison" should "handle compare operation" in {
    val result1 = ComplexDAL.compute("compare", 3, 5)
    result1 shouldBe a[java.lang.Integer]
    result1.asInstanceOf[Int] should be < 0 // 3 < 5

    val result2 = ComplexDAL.compute("compare", 7, 2)
    result2 shouldBe a[java.lang.Integer]
    result2.asInstanceOf[Int] should be > 0 // 7 > 2

    val result3 = ComplexDAL.compute("compare", 4, 4)
    result3 shouldBe a[java.lang.Integer]
    result3.asInstanceOf[Int] shouldBe 0 // 4 == 4
  }

  it should "handle rational comparisons" in {
    val result = ComplexDAL.compute("compare", Rational(1, 2), Rational(2, 3))
    result.asInstanceOf[Int] should be < 0 // 1/2 < 2/3
  }

  "ComplexDAL mathematical functions" should "handle square root function" in {
    // Perfect square
    ComplexDAL.sqrtFunction(4) shouldBe 2
    ComplexDAL.sqrtFunction(9) shouldBe 3

    // Non-perfect square
    val result1 = ComplexDAL.sqrtFunction(2)
    result1 shouldBe a[java.lang.Double]
    result1.asInstanceOf[Double] shouldBe math.sqrt(2) +- 0.001

    // Negative number should give complex result
    val result2 = ComplexDAL.sqrtFunction(-4)
    result2 shouldBe a[ComplexBigInt]
    val complex = result2.asInstanceOf[ComplexBigInt]
    complex.re shouldBe BigInt(0)
    complex.im shouldBe BigInt(2)
  }

  it should "handle absolute value function" in {
    ComplexDAL.absFunction(-5) shouldBe 5
    ComplexDAL.absFunction(7) shouldBe 7

    val result = ComplexDAL.absFunction(ComplexDouble(3, 4))
    result shouldBe a[java.lang.Double]
    result.asInstanceOf[Double] shouldBe 5.0 +- 0.001 // √(3² + 4²) = 5
  }

  it should "handle logarithm function" in {
    val result1 = ComplexDAL.lnFunction(math.E)
    result1 shouldBe a[java.lang.Double]
    result1.asInstanceOf[Double] shouldBe 1.0 +- 0.001

    val result2 = ComplexDAL.lnFunction(1)
    result2 shouldBe a[java.lang.Double]
    result2.asInstanceOf[Double] shouldBe 0.0 +- 0.001
  }

  it should "handle trigonometric functions" in {
    val result1 = ComplexDAL.sinFunction(math.Pi / 2)
    result1.asInstanceOf[Double] shouldBe 1.0 +- 0.001

    val result2 = ComplexDAL.cosFunction(0)
    result2.asInstanceOf[Double] shouldBe 1.0 +- 0.001

    val result3 = ComplexDAL.tanFunction(math.Pi / 4)
    result3.asInstanceOf[Double] shouldBe 1.0 +- 0.001
  }

  it should "handle floor and ceiling functions" in {
    ComplexDAL.floorFunction(3.7) shouldBe 3
    ComplexDAL.floorFunction(-2.3) shouldBe -3

    ComplexDAL.ceilFunction(3.2) shouldBe 4
    ComplexDAL.ceilFunction(-2.8) shouldBe -2

    // Rational floor/ceil
    val rational = Rational(7, 3) // 2.333...
    ComplexDAL.floorFunction(rational) shouldBe Rational(2)
    ComplexDAL.ceilFunction(rational) shouldBe Rational(3)
  }

  "ComplexDAL type preservation and optimization" should "preserve exact arithmetic when possible" in {
    // Rational arithmetic should stay exact
    val result1 = eval("1/3 + 1/6", ComplexDAL)
    result1 shouldBe a[SmallRational]
    result1.asInstanceOf[SmallRational] shouldBe SmallRational(1L, 2L)

    // Integer arithmetic should stay exact
    eval("1000000 * 1000000", ComplexDAL) shouldBe BigInt("1000000000000")

    // Complex rational should stay exact when possible
    val result2 = eval("1/2+1/3i * 2+0i", ComplexDAL)
    result2 shouldBe a[ComplexRational]
    val cr = result2.asInstanceOf[ComplexRational]
    cr.re shouldBe Rational(1)    // (1/2) * 2 = 1
    cr.im shouldBe Rational(2, 3) // (1/3) * 2 = 2/3
  }

  it should "promote integers to complex when needed" in {
    // When mixed with complex, should promote to complex
    val result = eval("2 * 3+4i", ComplexDAL)
    result shouldBe a[ComplexDouble]
    result.asInstanceOf[ComplexDouble] shouldBe ComplexDouble(6, 8)
  }

  it should "demote complex numbers to real when imaginary part is zero" in {
    val result = eval("3+4i - 0+4i", ComplexDAL)
    // Should demote to real number since imaginary part becomes 0
    result shouldBe a[java.lang.Integer]
    result.asInstanceOf[Int] shouldBe 3
  }

  "ComplexDAL edge cases and error handling" should "handle division by zero appropriately" in {
    an[RuntimeException] should be thrownBy eval("1 / 0", ComplexDAL)
    an[RuntimeException] should be thrownBy eval("1+1i / 0+0i", ComplexDAL)
  }

  it should "handle very large numbers" in {
    val bigNum = "123456789012345678901234567890"
    val result = eval(s"$bigNum + 1", ComplexDAL)
    result shouldBe BigInt(bigNum) + 1
  }

  it should "handle overflow promotion for SmallRational" in {
    // Create SmallRationals that will overflow during arithmetic
    val large1 = SmallRational(Long.MaxValue / 2, 1L)
    val large2 = SmallRational(3L, 1L)

    val result = ComplexDAL.compute("+", large1, large2)
    // Should automatically promote to Rational on overflow
    result shouldBe a[Rational]
  }

  it should "handle very small rational numbers" in {
    val result = eval("1/1000000000 + 1/2000000000", ComplexDAL)
    result shouldBe a[SmallRational]
    val sr = result.asInstanceOf[SmallRational]
    sr.numerator shouldBe 3L
    sr.denominator shouldBe 2000000000L
  }

  it should "handle zero cases correctly" in {
    eval("0 + 0+0i", ComplexDAL) shouldBe 0
    eval("0+0i + 0", ComplexDAL) shouldBe 0
    eval("0 * 1+1i", ComplexDAL) shouldBe 0
  }

  it should "handle negative zero and special floating point values" in {
    eval("-0.0 + 0.0", ComplexDAL) shouldBe 0.0

    // Test with infinity and NaN if they can be parsed
    val infResult = ComplexDAL.compute("+", Double.PositiveInfinity, 1.0)
    infResult shouldBe Double.PositiveInfinity
  }
}
