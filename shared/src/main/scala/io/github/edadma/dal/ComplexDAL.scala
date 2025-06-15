package io.github.edadma.dal

import io.github.edadma.numbers.{BigDecimalMath, SmallRational}
import io.github.edadma.numbers.BigDecimalMath.decimal128.*

import java.lang as boxed
import scala.math.{BigInt, pow}

object ComplexDAL extends DAL {

  special(DoubleType, ComplexBigIntType, ComplexDoubleType)
  special(BigDecType, ComplexBigIntType, ComplexBigDecType)
  special(RationalType, ComplexBigIntType, ComplexRationalType)
  special(SmallRationalType, ComplexBigIntType, ComplexRationalType)
  special(DoubleType, ComplexRationalType, ComplexDoubleType)
  special(DoubleType, ComplexRationalType, ComplexDoubleType)
  special(BigDecType, ComplexRationalType, ComplexBigDecType)
  special(BigDecType, ComplexDoubleType, ComplexBigDecType)

  operation(
    Symbol("+"),
    IntType             -> ((l: Number, r: Number) => maybePromote(l.longValue + r.longValue)),
    LongType            -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) + toBigInt(r))),
    BigIntType          -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) + toBigInt(r))),
    SmallRationalType   -> ((l: Number, r: Number) => safeSmallRationalOp(l, r, SmallRational.safeAdd, _ + _)),
    RationalType        -> ((l: Number, r: Number) => maybeDemote(toRational(l) + toRational(r))),
    DoubleType          -> ((l: Number, r: Number) => (DoubleType, l.doubleValue + r.doubleValue: Number)),
    BigDecType          -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) + toBigDecimal(b))),
    ComplexBigIntType   -> ((l: Number, r: Number) => maybeDemote(toComplexBigInt(l) + toComplexBigInt(r))),
    ComplexRationalType -> ((l: Number, r: Number) => maybeDemote(toComplexRational(l) + toComplexRational(r))),
    ComplexDoubleType   -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) + toComplexDouble(r))),
    ComplexBigDecType -> ((l: Number, r: Number) =>
      (ComplexBigDecType, toComplexBigDecimal(l) + toComplexBigDecimal(r))
    ),
  )

  operation(
    Symbol("-"),
    IntType             -> ((l: Number, r: Number) => maybePromote(l.longValue - r.longValue)),
    LongType            -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) - toBigInt(r))),
    BigIntType          -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) - toBigInt(r))),
    SmallRationalType   -> ((l: Number, r: Number) => safeSmallRationalOp(l, r, SmallRational.safeSubtract, _ - _)),
    RationalType        -> ((l: Number, r: Number) => maybeDemote(toRational(l) - toRational(r))),
    DoubleType          -> ((l: Number, r: Number) => (DoubleType, l.doubleValue - r.doubleValue: Number)),
    BigDecType          -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) - toBigDecimal(b))),
    ComplexBigIntType   -> ((l: Number, r: Number) => maybeDemote(toComplexBigInt(l) - toComplexBigInt(r))),
    ComplexRationalType -> ((l: Number, r: Number) => maybeDemote(toComplexRational(l) - toComplexRational(r))),
    ComplexDoubleType   -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) - toComplexDouble(r))),
    ComplexBigDecType -> ((l: Number, r: Number) =>
      (ComplexBigDecType, toComplexBigDecimal(l) - toComplexBigDecimal(r))
    ),
  )

  operation(
    Symbol("*"),
    IntType             -> ((l: Number, r: Number) => maybePromote(l.longValue * r.longValue)),
    LongType            -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) * toBigInt(r))),
    BigIntType          -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) * toBigInt(r))),
    SmallRationalType   -> ((l: Number, r: Number) => safeSmallRationalOp(l, r, SmallRational.safeMultiply, _ * _)),
    RationalType        -> ((l: Number, r: Number) => maybeDemote(toRational(l) * toRational(r))),
    DoubleType          -> ((l: Number, r: Number) => (DoubleType, l.doubleValue * r.doubleValue: Number)),
    BigDecType          -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) * toBigDecimal(b))),
    ComplexBigIntType   -> ((l: Number, r: Number) => maybeDemote(toComplexBigInt(l) * toComplexBigInt(r))),
    ComplexRationalType -> ((l: Number, r: Number) => maybeDemote(toComplexRational(l) * toComplexRational(r))),
    ComplexDoubleType   -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) * toComplexDouble(r))),
    ComplexBigDecType -> ((l: Number, r: Number) =>
      (ComplexBigDecType, toComplexBigDecimal(l) * toComplexBigDecimal(r))
    ),
  )

  operation(
    Symbol("/"),
    IntType -> ((l: Number, r: Number) => {
      try {
        val result = SmallRational(l.intValue.toLong, r.intValue.toLong)
        maybeDemote(result)
      } catch {
        case _: ArithmeticException => maybeDemote(toRational(l) / toRational(r))
      }
    }),
    LongType -> ((l: Number, r: Number) => {
      try {
        val result = SmallRational(l.longValue, r.longValue)
        maybeDemote(result)
      } catch {
        case _: ArithmeticException => maybeDemote(toRational(l) / toRational(r))
      }
    }),
    BigIntType          -> ((l: Number, r: Number) => maybeDemote(toRational(l) / toRational(r))),
    SmallRationalType   -> ((l: Number, r: Number) => safeSmallRationalOp(l, r, SmallRational.safeDivide, _ / _)),
    RationalType        -> ((l: Number, r: Number) => maybeDemote(toRational(l) / toRational(r))),
    DoubleType          -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    BigDecType          -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) / toBigDecimal(b))),
    ComplexBigIntType   -> ((l: Number, r: Number) => maybeDemote(toComplexRational(l) / toComplexRational(r))),
    ComplexRationalType -> ((l: Number, r: Number) => maybeDemote(toComplexRational(l) / toComplexRational(r))),
    ComplexDoubleType   -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) / toComplexDouble(r))),
    ComplexBigDecType -> ((l: Number, r: Number) =>
      (ComplexBigDecType, toComplexBigDecimal(l) / toComplexBigDecimal(r))
    ),
  )

  operation(
    Symbol("//"),
    IntType             -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    LongType            -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    BigIntType          -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    SmallRationalType   -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    RationalType        -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    DoubleType          -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    BigDecType          -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) / toBigDecimal(b))),
    ComplexBigIntType   -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) / toComplexDouble(r))),
    ComplexRationalType -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) / toComplexDouble(r))),
    ComplexDoubleType   -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) / toComplexDouble(r))),
    ComplexBigDecType -> ((l: Number, r: Number) =>
      (ComplexBigDecType, toComplexBigDecimal(l) / toComplexBigDecimal(r))
    ),
  )

  operation(
    Symbol("^"),
    IntType    -> ((l: Number, r: Number) => bigIntPow(l, r)),
    LongType   -> ((l: Number, r: Number) => bigIntPow(l, r)),
    BigIntType -> ((l: Number, r: Number) => bigIntPow(l, r)),
    SmallRationalType -> ((l: Number, r: Number) => {
      r match {
        case i: boxed.Integer => maybeDemote(toSmallRational(l) ^ i)
        case bi: BigInt       => (RationalType, toRational(l) ^ bi)
        case _                => (DoubleType, pow(l.doubleValue, r.doubleValue): Number)
      }
    }),
    RationalType -> ((l: Number, r: Number) => {
      r match {
        case i: boxed.Integer => maybeDemote(toRational(l) ^ i)
        case bi: BigInt       => (RationalType, toRational(l) ^ bi)
        case _                => (DoubleType, pow(l.doubleValue, r.doubleValue): Number)
      }
    }),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, pow(l.doubleValue, r.doubleValue): Number)),
    BigDecType -> ((l: Number, r: Number) => (BigDecType, BigDecimalMath.pow(toBigDecimal(l), toBigDecimal(r)))),
    ComplexBigIntType -> ((l: Number, r: Number) =>
      r match {
        case p: boxed.Integer => (ComplexBigIntType, toComplexBigInt(l) ^ p)
        case p: boxed.Long    => (ComplexBigIntType, toComplexBigInt(l) ^ BigInt(p))
        case p: BigInt        => (ComplexBigIntType, toComplexBigInt(l) ^ p)
        case _                => (ComplexDoubleType, toComplexDouble(l) ^ toComplexDouble(r))
      }
    ),
    ComplexRationalType -> ((l: Number, r: Number) =>
      r match {
        case p: boxed.Integer => (ComplexRationalType, toComplexRational(l) ^ p)
        case p: boxed.Long    => (ComplexRationalType, toComplexRational(l) ^ BigInt(p))
        case p: BigInt        => (ComplexRationalType, toComplexRational(l) ^ p)
        case _                => (ComplexDoubleType, toComplexDouble(l) ^ toComplexDouble(r))
      }
    ),
    ComplexDoubleType -> ((l: Number, r: Number) => (ComplexDoubleType, toComplexDouble(l) ^ toComplexDouble(r))),
    ComplexBigDecType -> ((l: Number, r: Number) =>
      (ComplexBigDecType, toComplexBigDecimal(l) ^ toComplexBigDecimal(r))
    ),
  )

  operation(
    Symbol("mod"),
    IntType    -> ((l: Number, r: Number) => (IntType, l.intValue % r.intValue: Number)),
    LongType   -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) % toBigInt(r))),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) % toBigInt(r))),
    SmallRationalType -> ((l: Number, r: Number) => {
      // For SmallRational mod, convert to decimal for modulo operation
      (DoubleType, l.doubleValue % r.doubleValue: Number)
    }),
    RationalType -> ((l: Number, r: Number) => {
      // For Rational mod, convert to decimal for modulo operation
      (DoubleType, l.doubleValue % r.doubleValue: Number)
    }),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue % r.doubleValue: Number)),
    BigDecType -> ((l: Number, r: Number) => (BigDecType, toBigDecimal(l) % toBigDecimal(r))),
  )

  relation(
    Symbol("="),
    IntType             -> ((l: Number, r: Number) => boolean(l == r)),
    LongType            -> ((l: Number, r: Number) => boolean(l.longValue == r.longValue)),
    BigIntType          -> ((l: Number, r: Number) => boolean(toBigInt(l) == toBigInt(r))),
    SmallRationalType   -> ((l: Number, r: Number) => boolean(toSmallRational(l) == toSmallRational(r))),
    RationalType        -> ((l: Number, r: Number) => boolean(toRational(l) == toRational(r))),
    DoubleType          -> ((l: Number, r: Number) => boolean(l.doubleValue == r.doubleValue)),
    BigDecType          -> ((l: Number, r: Number) => boolean(toBigDecimal(l) == toBigDecimal(r))),
    ComplexBigIntType   -> ((l: Number, r: Number) => boolean(toComplexDouble(l) == toComplexDouble(r))),
    ComplexRationalType -> ((l: Number, r: Number) => boolean(toComplexDouble(l) == toComplexDouble(r))),
    ComplexDoubleType   -> ((l: Number, r: Number) => boolean(toComplexDouble(l) == toComplexDouble(r))),
    ComplexBigDecType   -> ((l: Number, r: Number) => boolean(toComplexBigDecimal(l) == toComplexBigDecimal(r))),
  )

  relation(
    Symbol("!="),
    IntType             -> ((l: Number, r: Number) => boolean(l != r)),
    LongType            -> ((l: Number, r: Number) => boolean(l.longValue != r.longValue)),
    BigIntType          -> ((l: Number, r: Number) => boolean(toBigInt(l) != toBigInt(r))),
    SmallRationalType   -> ((l: Number, r: Number) => boolean(toSmallRational(l) != toSmallRational(r))),
    RationalType        -> ((l: Number, r: Number) => boolean(toRational(l) != toRational(r))),
    DoubleType          -> ((l: Number, r: Number) => boolean(l.doubleValue != r.doubleValue)),
    BigDecType          -> ((l: Number, r: Number) => boolean(toBigDecimal(l) != toBigDecimal(r))),
    ComplexBigIntType   -> ((l: Number, r: Number) => boolean(toComplexDouble(l) != toComplexDouble(r))),
    ComplexRationalType -> ((l: Number, r: Number) => boolean(toComplexDouble(l) != toComplexDouble(r))),
    ComplexDoubleType   -> ((l: Number, r: Number) => boolean(toComplexDouble(l) != toComplexDouble(r))),
    ComplexBigDecType   -> ((l: Number, r: Number) => boolean(toComplexBigDecimal(l) != toComplexBigDecimal(r))),
  )

  relation(
    Symbol("<"),
    IntType           -> ((l: Number, r: Number) => boolean(l.intValue < r.intValue)),
    LongType          -> ((l: Number, r: Number) => boolean(l.longValue < r.longValue)),
    BigIntType        -> ((l: Number, r: Number) => boolean(toBigInt(l) < toBigInt(r))),
    SmallRationalType -> ((l: Number, r: Number) => boolean(toSmallRational(l) < toSmallRational(r))),
    RationalType      -> ((l: Number, r: Number) => boolean(toRational(l) < toRational(r))),
    DoubleType        -> ((l: Number, r: Number) => boolean(l.doubleValue < r.doubleValue)),
    BigDecType        -> ((l: Number, r: Number) => boolean(toBigDecimal(l) < toBigDecimal(r))),
  )

  relation(
    Symbol(">"),
    IntType           -> ((l: Number, r: Number) => boolean(l.intValue > r.intValue)),
    LongType          -> ((l: Number, r: Number) => boolean(l.longValue > r.longValue)),
    BigIntType        -> ((l: Number, r: Number) => boolean(toBigInt(l) > toBigInt(r))),
    SmallRationalType -> ((l: Number, r: Number) => boolean(toSmallRational(l) > toSmallRational(r))),
    RationalType      -> ((l: Number, r: Number) => boolean(toRational(l) > toRational(r))),
    DoubleType        -> ((l: Number, r: Number) => boolean(l.doubleValue > r.doubleValue)),
    BigDecType        -> ((l: Number, r: Number) => boolean(toBigDecimal(l) > toBigDecimal(r))),
  )

  relation(
    Symbol("<="),
    IntType           -> ((l: Number, r: Number) => boolean(l.intValue <= r.intValue)),
    LongType          -> ((l: Number, r: Number) => boolean(l.longValue <= r.longValue)),
    BigIntType        -> ((l: Number, r: Number) => boolean(toBigInt(l) <= toBigInt(r))),
    SmallRationalType -> ((l: Number, r: Number) => boolean(toSmallRational(l) <= toSmallRational(r))),
    RationalType      -> ((l: Number, r: Number) => boolean(toRational(l) <= toRational(r))),
    DoubleType        -> ((l: Number, r: Number) => boolean(l.doubleValue <= r.doubleValue)),
    BigDecType        -> ((l: Number, r: Number) => boolean(toBigDecimal(l) <= toBigDecimal(r))),
  )

  relation(
    Symbol(">="),
    IntType           -> ((l: Number, r: Number) => boolean(l.intValue >= r.intValue)),
    LongType          -> ((l: Number, r: Number) => boolean(l.longValue >= r.longValue)),
    BigIntType        -> ((l: Number, r: Number) => boolean(toBigInt(l) >= toBigInt(r))),
    SmallRationalType -> ((l: Number, r: Number) => boolean(toSmallRational(l) >= toSmallRational(r))),
    RationalType      -> ((l: Number, r: Number) => boolean(toRational(l) >= toRational(r))),
    DoubleType        -> ((l: Number, r: Number) => boolean(l.doubleValue >= r.doubleValue)),
    BigDecType        -> ((l: Number, r: Number) => boolean(toBigDecimal(l) >= toBigDecimal(r))),
  )

  relation(
    Symbol("div"),
    IntType    -> ((l, r) => boolean(r.intValue % l.intValue == 0)),
    LongType   -> ((l: Number, r: Number) => boolean(r.longValue % l.longValue == 0)),
    BigIntType -> ((l: Number, r: Number) => boolean(toBigInt(r) % toBigInt(l) == 0)),
    SmallRationalType -> ((l: Number, r: Number) => {
      // For rational numbers, check if r/l is a whole number
      val result = toSmallRational(r) / toSmallRational(l)
      boolean(result.isWhole)
    }),
    RationalType -> ((l: Number, r: Number) => {
      // For rational numbers, check if r/l is a whole number
      val result = toRational(r) / toRational(l)
      boolean(result.isWhole)
    }),
    // complex int types - TODO: implement divisibility for complex integers
  )

  operation(
    Symbol("\\"),
    IntType    -> ((l, r) => (IntType, l.intValue / r.intValue: Number)),
    LongType   -> ((l: Number, r: Number) => (LongType, l.longValue / r.longValue: Number)),
    BigIntType -> ((l: Number, r: Number) => (BigIntType, toBigInt(l) / toBigInt(r))),
    SmallRationalType -> ((l: Number, r: Number) => {
      // Integer division for rationals - return the quotient part
      val result = toSmallRational(l) / toSmallRational(r)
      if (result.isWhole) {
        if (result.numerator >= Int.MinValue && result.numerator <= Int.MaxValue) {
          (IntType, result.numerator.toInt.asInstanceOf[Number])
        } else {
          (LongType, result.numerator.asInstanceOf[Number])
        }
      } else {
        (IntType, result.intValue.asInstanceOf[Number])
      }
    }),
    RationalType -> ((l: Number, r: Number) => {
      // Integer division for rationals - return the quotient part
      val result = toRational(l) / toRational(r)
      if (result.isWhole) {
        if (result.numerator.isValidInt) {
          (IntType, result.numerator.toInt.asInstanceOf[Number])
        } else if (result.numerator.isValidLong) {
          (LongType, result.numerator.toLong.asInstanceOf[Number])
        } else {
          (BigIntType, result.numerator)
        }
      } else {
        (IntType, result.intValue.asInstanceOf[Number])
      }
    }),
    // complex int types - TODO: implement integer division for complex integers
  )

  operation(
    Symbol("and"),
    IntType    -> ((l: Number, r: Number) => (IntType, l.intValue & r.intValue: Number)),
    LongType   -> ((l: Number, r: Number) => (IntType, l.longValue & r.longValue: Number)),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) & toBigInt(r))),
    // Note: bitwise operations don't make sense for rational/decimal numbers
  )

  operation(
    Symbol("or"),
    IntType    -> ((l: Number, r: Number) => (IntType, l.intValue | r.intValue: Number)),
    LongType   -> ((l: Number, r: Number) => (IntType, l.longValue | r.longValue: Number)),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) | toBigInt(r))),
    // Note: bitwise operations don't make sense for rational/decimal numbers
  )

  operation(
    Symbol("xor"),
    IntType    -> ((l: Number, r: Number) => (IntType, l.intValue ^ r.intValue: Number)),
    LongType   -> ((l: Number, r: Number) => (IntType, l.longValue ^ r.longValue: Number)),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) ^ toBigInt(r))),
    // Note: bitwise operations don't make sense for rational/decimal numbers
  )

  operation(
    Symbol("compare"),
    IntType           -> ((l: Number, r: Number) => (IntType, l.intValue.compare(r.intValue): Number)),
    LongType          -> ((l: Number, r: Number) => (IntType, l.longValue.compare(r.longValue): Number)),
    BigIntType        -> ((l: Number, r: Number) => (IntType, toBigInt(l).compare(toBigInt(r)): Number)),
    SmallRationalType -> ((l: Number, r: Number) => (IntType, toSmallRational(l).compare(toSmallRational(r)): Number)),
    RationalType      -> ((l: Number, r: Number) => (IntType, toRational(l).compare(toRational(r)): Number)),
    DoubleType        -> ((l: Number, r: Number) => (IntType, l.doubleValue.compare(r.doubleValue): Number)),
    BigDecType        -> ((l: Number, r: Number) => (IntType, toBigDecimal(l).compare(toBigDecimal(r)): Number)),
  )

}
