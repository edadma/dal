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

  "ComplexDAL comparisons" should "handle real number comparisons" in {
    eval("3 = 3", ComplexDAL) shouldBe true
    eval("3 = 4", ComplexDAL) shouldBe false
    eval("5 > 3", ComplexDAL) shouldBe true
    eval("2 < 7", ComplexDAL) shouldBe true
    eval("1/2 = 0.5", ComplexDAL) shouldBe true
  }

  it should "handle complex number equality" in {
    eval("3+4i = 3+4i", ComplexDAL) shouldBe true
    eval("3+4i = 3+5i", ComplexDAL) shouldBe false
    eval("2+0i = 2", ComplexDAL) shouldBe true // Real number should equal complex with 0 imaginary
  }

  "ComplexDAL power operations" should "handle real number powers" in {
    eval("2 ^ 3", ComplexDAL) shouldBe BigInt(8)
    eval("3 ^ 2", ComplexDAL) shouldBe BigInt(9)

    val result = eval("1/2 ^ 2", ComplexDAL)
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(1L, 4L)
  }

  it should "handle complex number powers" in {
    // i^2 = -1
    val result = eval("0+1i ^ 2", ComplexDAL)
    result shouldBe a[ComplexDouble]
    val complex = result.asInstanceOf[ComplexDouble]
    complex.re shouldBe -1.0 +- 0.001
    complex.im shouldBe 0.0 +- 0.001
  }

  "ComplexDAL type promotion" should "promote integers to complex when needed" in {
    // When mixed with complex, should promote to complex
    val result = eval("2 * 3+4i", ComplexDAL)
    result shouldBe a[ComplexDouble]
    result.asInstanceOf[ComplexDouble] shouldBe ComplexDouble(6, 8)
  }

  it should "preserve exact arithmetic when possible" in {
    // Rational arithmetic should stay exact
    val result1 = eval("1/3 + 1/6", ComplexDAL)
    result1 shouldBe a[SmallRational]
    result1.asInstanceOf[SmallRational] shouldBe SmallRational(1L, 2L)

    // Integer arithmetic should stay exact
    eval("1000000 * 1000000", ComplexDAL) shouldBe BigInt("1000000000000")
  }

  "ComplexDAL float division" should "provide floating point division when requested" in {
    eval("1 // 3", ComplexDAL) shouldBe (1.0 / 3.0)
    eval("7 // 2", ComplexDAL) shouldBe 3.5

    // Complex float division
    val result = eval("1+1i // 2+0i", ComplexDAL)
    result shouldBe a[ComplexDouble]
    result.asInstanceOf[ComplexDouble] shouldBe ComplexDouble(0.5, 0.5)
  }

  "ComplexDAL edge cases" should "handle division by zero appropriately" in {
    an[RuntimeException] should be thrownBy eval("1 / 0", ComplexDAL)
    an[RuntimeException] should be thrownBy eval("1+1i / 0+0i", ComplexDAL)
  }

  it should "handle very large numbers" in {
    val bigNum = "123456789012345678901234567890"
    val result = eval(s"$bigNum + 1", ComplexDAL)
    result shouldBe BigInt(bigNum) + 1
  }
}
