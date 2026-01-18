package io.github.edadma.dal

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.github.edadma.numbers._

class QuaternionDALTest extends AnyFlatSpec with Matchers {

  "QuaternionDAL SmallRational arithmetic" should "handle SmallRational addition" in {
    val result = eval("1/2 + 1/3", QuaternionDAL)
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(5L, 6L)
  }

  it should "handle SmallRational subtraction" in {
    val result = eval("3/4 - 1/4", QuaternionDAL)
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(1L, 2L)
  }

  it should "handle SmallRational multiplication" in {
    val result = eval("2/3 * 3/4", QuaternionDAL)
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(1L, 2L)
  }

  it should "handle SmallRational division" in {
    val result = eval("1/2 / 1/4", QuaternionDAL)
    result shouldBe a[java.lang.Integer]
    result.asInstanceOf[Int] shouldBe 2
  }

  it should "handle SmallRational float division" in {
    val result = eval("3/4 // 1/2", QuaternionDAL)
    result shouldBe a[java.lang.Double]
    result.asInstanceOf[Double] shouldBe 1.5 +- 0.001
  }

  it should "handle SmallRational power with integer exponent" in {
    val result = eval("1/2 ^ 3", QuaternionDAL)
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(1L, 8L)
  }

  "QuaternionDAL SmallRational comparisons" should "handle equality" in {
    eval("1/2 = 1/2", QuaternionDAL) shouldBe true
    eval("1/2 = 1/3", QuaternionDAL) shouldBe false
  }

  it should "handle inequality" in {
    eval("1/2 != 1/3", QuaternionDAL) shouldBe true
    eval("1/2 != 1/2", QuaternionDAL) shouldBe false
  }

  it should "handle less than" in {
    eval("1/3 < 1/2", QuaternionDAL) shouldBe true
    eval("1/2 < 1/3", QuaternionDAL) shouldBe false
  }

  it should "handle greater than" in {
    eval("1/2 > 1/3", QuaternionDAL) shouldBe true
    eval("1/3 > 1/2", QuaternionDAL) shouldBe false
  }

  it should "handle less than or equal" in {
    eval("1/3 <= 1/2", QuaternionDAL) shouldBe true
    eval("1/2 <= 1/2", QuaternionDAL) shouldBe true
    eval("1/2 <= 1/3", QuaternionDAL) shouldBe false
  }

  it should "handle greater than or equal" in {
    eval("1/2 >= 1/3", QuaternionDAL) shouldBe true
    eval("1/2 >= 1/2", QuaternionDAL) shouldBe true
    eval("1/3 >= 1/2", QuaternionDAL) shouldBe false
  }

  it should "handle compare operation" in {
    val result1 = QuaternionDAL.compute("compare", SmallRational(1L, 3L), SmallRational(1L, 2L))
    result1.asInstanceOf[Int] should be < 0

    val result2 = QuaternionDAL.compute("compare", SmallRational(1L, 2L), SmallRational(1L, 3L))
    result2.asInstanceOf[Int] should be > 0

    val result3 = QuaternionDAL.compute("compare", SmallRational(1L, 2L), SmallRational(1L, 2L))
    result3.asInstanceOf[Int] shouldBe 0
  }

  "QuaternionDAL SmallRational special promotions" should "promote SmallRational + ComplexBigInt to ComplexRational" in {
    val complexBigInt = ComplexBigInt(BigInt(3), BigInt(4))
    val result = QuaternionDAL.compute("+", SmallRational(1L, 2L), complexBigInt)
    result shouldBe a[ComplexRational]
    val cr = result.asInstanceOf[ComplexRational]
    cr.re shouldBe Rational(7, 2) // 3 + 1/2 = 7/2
    cr.im shouldBe Rational(4)
  }

  it should "promote SmallRational + QuaternionBigInt to QuaternionRational" in {
    val quaternionBigInt = QuaternionBigInt(BigInt(1), BigInt(2), BigInt(3), BigInt(4))
    val result = QuaternionDAL.compute("+", SmallRational(1L, 2L), quaternionBigInt)
    result shouldBe a[QuaternionRational]
    val qr = result.asInstanceOf[QuaternionRational]
    qr.a shouldBe Rational(3, 2) // 1 + 1/2 = 3/2
    qr.b shouldBe Rational(2)
    qr.c shouldBe Rational(3)
    qr.d shouldBe Rational(4)
  }

  "QuaternionDAL SmallRational overflow" should "handle overflow by promoting to Rational" in {
    val large1 = SmallRational(Long.MaxValue, 1L)
    val large2 = SmallRational(1L, 1L)

    val result = QuaternionDAL.compute("+", large1, large2)
    // Result overflows Long, so gets promoted to BigInt (correct demotion!)
    result shouldBe a[BigInt]
    result.asInstanceOf[BigInt] shouldBe BigInt("9223372036854775808")
  }

  "QuaternionDAL basic real number operations" should "work like other DALs" in {
    eval("3 + 4", QuaternionDAL) shouldBe 7
    eval("10 - 3", QuaternionDAL) shouldBe 7
    eval("6 * 7", QuaternionDAL) shouldBe 42

    val result = eval("1 / 3", QuaternionDAL)
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(1L, 3L)
  }

  "QuaternionDAL SmallRational mixed operations" should "handle SmallRational + Integer" in {
    val result = QuaternionDAL.compute("+", SmallRational(1L, 2L), 3: Integer)
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(7L, 2L)
  }

  it should "handle Integer + SmallRational" in {
    val result = QuaternionDAL.compute("+", 3: Integer, SmallRational(1L, 2L))
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(7L, 2L)
  }

  it should "handle SmallRational + Double promotes to Double" in {
    val result = QuaternionDAL.compute("+", SmallRational(1L, 2L), 0.25: java.lang.Double)
    result shouldBe a[java.lang.Double]
    result.asInstanceOf[Double] shouldBe 0.75 +- 0.001
  }

  it should "handle SmallRational * Integer" in {
    val result = QuaternionDAL.compute("*", SmallRational(2L, 3L), 6: Integer)
    result shouldBe a[java.lang.Integer]
    result.asInstanceOf[Int] shouldBe 4
  }

  it should "handle SmallRational - SmallRational with negative result" in {
    val result = QuaternionDAL.compute("-", SmallRational(1L, 4L), SmallRational(1L, 2L))
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(-1L, 4L)
  }

  "QuaternionDAL SmallRational edge cases" should "handle zero numerator" in {
    val result = QuaternionDAL.compute("+", SmallRational(0L, 1L), SmallRational(1L, 2L))
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(1L, 2L)
  }

  it should "handle multiplication by zero" in {
    val result = QuaternionDAL.compute("*", SmallRational(1L, 2L), 0: Integer)
    result shouldBe 0
  }

  it should "handle negative SmallRational" in {
    val result = eval("-1/2 + -1/3", QuaternionDAL)
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(-5L, 6L)
  }

  it should "handle negative SmallRational comparisons" in {
    eval("-1/2 < 1/2", QuaternionDAL) shouldBe true
    eval("-1/2 > -1/3", QuaternionDAL) shouldBe false
  }

  "QuaternionDAL SmallRational negate" should "negate SmallRational" in {
    val result = QuaternionDAL.negate(SmallRational(3L, 4L))
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(-3L, 4L)
  }

  it should "negate negative SmallRational" in {
    val result = QuaternionDAL.negate(SmallRational(-3L, 4L))
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(3L, 4L)
  }

  "QuaternionDAL SmallRational math functions" should "handle sqrt of SmallRational" in {
    val result = QuaternionDAL.sqrtFunction(SmallRational(1L, 4L))
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(1L, 2L)
  }

  it should "handle sqrt of non-perfect square SmallRational" in {
    val result = QuaternionDAL.sqrtFunction(SmallRational(1L, 2L))
    result shouldBe a[java.lang.Double]
    result.asInstanceOf[Double] shouldBe math.sqrt(0.5) +- 0.001
  }

  it should "handle sin of SmallRational" in {
    val result = QuaternionDAL.sinFunction(SmallRational(0L, 1L))
    result shouldBe a[java.lang.Double]
    result.asInstanceOf[Double] shouldBe 0.0 +- 0.001
  }

  it should "handle cos of SmallRational" in {
    val result = QuaternionDAL.cosFunction(SmallRational(0L, 1L))
    result shouldBe a[java.lang.Double]
    result.asInstanceOf[Double] shouldBe 1.0 +- 0.001
  }

  it should "handle exp of SmallRational" in {
    val result = QuaternionDAL.expFunction(SmallRational(0L, 1L))
    result shouldBe a[java.lang.Double]
    result.asInstanceOf[Double] shouldBe 1.0 +- 0.001
  }

  it should "handle ln of SmallRational" in {
    val result = QuaternionDAL.lnFunction(SmallRational(1L, 1L))
    result shouldBe a[java.lang.Double]
    result.asInstanceOf[Double] shouldBe 0.0 +- 0.001
  }

  it should "handle abs of negative SmallRational" in {
    val result = QuaternionDAL.absFunction(SmallRational(-3L, 4L))
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(3L, 4L)
  }

  it should "handle floor of SmallRational" in {
    val result = QuaternionDAL.floorFunction(SmallRational(7L, 3L))
    result shouldBe Rational(2)
  }

  it should "handle ceil of SmallRational" in {
    val result = QuaternionDAL.ceilFunction(SmallRational(7L, 3L))
    result shouldBe Rational(3)
  }

  "QuaternionDAL SmallRational quaternion promotions" should "promote SmallRational + QuaternionDouble to QuaternionDouble" in {
    val qd = QuaternionDouble(1.0, 2.0, 3.0, 4.0)
    val result = QuaternionDAL.compute("+", SmallRational(1L, 2L), qd)
    result shouldBe a[QuaternionDouble]
    val r = result.asInstanceOf[QuaternionDouble]
    r.a shouldBe 1.5 +- 0.001
    r.b shouldBe 2.0 +- 0.001
    r.c shouldBe 3.0 +- 0.001
    r.d shouldBe 4.0 +- 0.001
  }

  it should "promote SmallRational + QuaternionRational to QuaternionRational" in {
    val qr = QuaternionRational(Rational(1), Rational(2), Rational(3), Rational(4))
    val result = QuaternionDAL.compute("+", SmallRational(1L, 2L), qr)
    result shouldBe a[QuaternionRational]
    val r = result.asInstanceOf[QuaternionRational]
    r.a shouldBe Rational(3, 2)
    r.b shouldBe Rational(2)
    r.c shouldBe Rational(3)
    r.d shouldBe Rational(4)
  }

  "QuaternionDAL SmallRational complex promotions" should "promote SmallRational + ComplexDouble to ComplexDouble" in {
    val cd = ComplexDouble(1.0, 2.0)
    val result = QuaternionDAL.compute("+", SmallRational(1L, 2L), cd)
    result shouldBe a[ComplexDouble]
    val r = result.asInstanceOf[ComplexDouble]
    r.re shouldBe 1.5 +- 0.001
    r.im shouldBe 2.0 +- 0.001
  }

  it should "promote SmallRational + ComplexRational to ComplexRational" in {
    val cr = ComplexRational(Rational(1), Rational(2))
    val result = QuaternionDAL.compute("+", SmallRational(1L, 2L), cr)
    result shouldBe a[ComplexRational]
    val r = result.asInstanceOf[ComplexRational]
    r.re shouldBe Rational(3, 2)
    r.im shouldBe Rational(2)
  }

  "QuaternionDAL SmallRational power edge cases" should "handle SmallRational ^ 0" in {
    val result = QuaternionDAL.compute("^", SmallRational(3L, 4L), 0: Integer)
    result shouldBe 1
  }

  it should "handle SmallRational ^ 1" in {
    val result = QuaternionDAL.compute("^", SmallRational(3L, 4L), 1: Integer)
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(3L, 4L)
  }

  it should "handle SmallRational ^ negative integer" in {
    val result = QuaternionDAL.compute("^", SmallRational(2L, 1L), -2: Integer)
    result shouldBe a[SmallRational]
    result.asInstanceOf[SmallRational] shouldBe SmallRational(1L, 4L)
  }

  it should "handle SmallRational ^ Double" in {
    val result = QuaternionDAL.compute("^", SmallRational(1L, 4L), 0.5: java.lang.Double)
    result shouldBe a[java.lang.Double]
    result.asInstanceOf[Double] shouldBe 0.5 +- 0.001
  }
}
