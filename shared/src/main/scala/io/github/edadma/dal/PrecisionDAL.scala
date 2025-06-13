package io.github.edadma.dal

import java.lang as boxed
import scala.math.{BigInt, pow}
import io.github.edadma.numbers.{BigDecimalMath, SmallRational}
import BigDecimalMath.decimal128.*

object PrecisionDAL extends DAL {

  operation(
    Symbol("+"),
    IntType           -> ((l: Number, r: Number) => maybePromote(l.longValue + r.longValue)),
    LongType          -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) + toBigInt(r))),
    BigIntType        -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) + toBigInt(r))),
    SmallRationalType -> ((l: Number, r: Number) => safeSmallRationalOp(l, r, SmallRational.safeAdd, _ + _)),
    RationalType      -> ((l: Number, r: Number) => maybeDemote(toRational(l) + toRational(r))),
    DoubleType        -> ((l: Number, r: Number) => (DoubleType, l.doubleValue + r.doubleValue: Number)),
    BigDecType        -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) + toBigDecimal(b))),
  )

  operation(
    Symbol("-"),
    IntType           -> ((l: Number, r: Number) => maybePromote(l.longValue - r.longValue)),
    LongType          -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) - toBigInt(r))),
    BigIntType        -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) - toBigInt(r))),
    SmallRationalType -> ((l: Number, r: Number) => safeSmallRationalOp(l, r, SmallRational.safeSubtract, _ - _)),
    RationalType      -> ((l: Number, r: Number) => maybeDemote(toRational(l) - toRational(r))),
    DoubleType        -> ((l: Number, r: Number) => (DoubleType, l.doubleValue - r.doubleValue: Number)),
    BigDecType        -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) - toBigDecimal(b))),
  )

  operation(
    Symbol("*"),
    IntType           -> ((l: Number, r: Number) => maybePromote(l.longValue * r.longValue)),
    LongType          -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) * toBigInt(r))),
    BigIntType        -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) * toBigInt(r))),
    SmallRationalType -> ((l: Number, r: Number) => safeSmallRationalOp(l, r, SmallRational.safeMultiply, _ * _)),
    RationalType      -> ((l: Number, r: Number) => maybeDemote(toRational(l) * toRational(r))),
    DoubleType        -> ((l: Number, r: Number) => (DoubleType, l.doubleValue * r.doubleValue: Number)),
    BigDecType        -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) * toBigDecimal(b))),
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
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toRational(l) / toRational(r))),
    SmallRationalType -> ((l: Number, r: Number) =>
      safeSmallRationalOp(l, r, SmallRational.safeDivide, _ / _)
    ),
    RationalType -> ((l: Number, r: Number) => maybeDemote(toRational(l) / toRational(r))),
    DoubleType   -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    BigDecType   -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) / toBigDecimal(b))),
  )

  operation(
    Symbol("//"),
    IntType      -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    LongType     -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    BigIntType   -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    RationalType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    DoubleType   -> ((l: Number, r: Number) => (DoubleType, l.doubleValue / r.doubleValue: Number)),
    BigDecType   -> ((a: Number, b: Number) => (BigDecType, toBigDecimal(a) / toBigDecimal(b))),
  )

  operation(
    Symbol("^"),
    IntType    -> ((l: Number, r: Number) => bigIntPow(l, r)),
    LongType   -> ((l: Number, r: Number) => bigIntPow(l, r)),
    BigIntType -> ((l: Number, r: Number) => bigIntPow(l, r)),
    RationalType -> ((l: Number, r: Number) => {
      r match {
        case i: boxed.Integer => maybeDemote(toRational(l) ^ i)
        case bi: BigInt       => (RationalType, toRational(l) ^ bi)
        case _                => (DoubleType, pow(l.doubleValue, r.doubleValue): Number)
      }
    }),
    DoubleType -> ((l: Number, r: Number) => (DoubleType, pow(l.doubleValue, r.doubleValue): Number)),
    BigDecType -> ((l: Number, r: Number) => (BigDecType, BigDecimalMath.pow(toBigDecimal(l), toBigDecimal(r)))),
  )

  operation(
    Symbol("mod"),
    IntType    -> ((l: Number, r: Number) => (IntType, l.intValue % r.intValue: Number)),
    LongType   -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) % toBigInt(r))),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) % toBigInt(r))),
    // todo: rational
    DoubleType -> ((l: Number, r: Number) => (DoubleType, l.doubleValue % r.doubleValue: Number)),
    BigDecType -> ((l: Number, r: Number) => (BigDecType, toBigDecimal(l) % toBigDecimal(r))),
  )

  relation(
    Symbol("="),
    IntType           -> ((l: Number, r: Number) => boolean(l == r)),
    LongType          -> ((l: Number, r: Number) => boolean(l.longValue == r.longValue)),
    BigIntType        -> ((l: Number, r: Number) => boolean(toBigInt(l) == toBigInt(r))),
    SmallRationalType -> ((l: Number, r: Number) => boolean(toSmallRational(l) == toSmallRational(r))),
    RationalType      -> ((l: Number, r: Number) => boolean(toRational(l) == toRational(r))),
    DoubleType        -> ((l: Number, r: Number) => boolean(l.doubleValue == r.doubleValue)),
    BigDecType        -> ((l: Number, r: Number) => boolean(toBigDecimal(l) == toBigDecimal(r))),
  )

  relation(
    Symbol("!="),
    IntType           -> ((l: Number, r: Number) => boolean(l != r)),
    LongType          -> ((l: Number, r: Number) => boolean(l.longValue != r.longValue)),
    BigIntType        -> ((l: Number, r: Number) => boolean(toBigInt(l) != toBigInt(r))),
    SmallRationalType -> ((l: Number, r: Number) => boolean(toSmallRational(l) != toSmallRational(r))),
    RationalType      -> ((l: Number, r: Number) => boolean(toRational(l) != toRational(r))),
    DoubleType        -> ((l: Number, r: Number) => boolean(l.doubleValue != r.doubleValue)),
    BigDecType        -> ((l: Number, r: Number) => boolean(toBigDecimal(l) != toBigDecimal(r))),
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
  )

  operation(
    Symbol("\\"),
    IntType    -> ((l, r) => (IntType, l.intValue / r.intValue: Number)),
    LongType   -> ((l: Number, r: Number) => (LongType, l.longValue / r.longValue: Number)),
    BigIntType -> ((l: Number, r: Number) => (BigIntType, toBigInt(l) / toBigInt(r))),
  )

  operation(
    Symbol("and"),
    IntType    -> ((l: Number, r: Number) => (IntType, l.intValue & r.intValue: Number)),
    LongType   -> ((l: Number, r: Number) => (IntType, l.longValue & r.longValue: Number)),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) & toBigInt(r))),
  )

  operation(
    Symbol("or"),
    IntType    -> ((l: Number, r: Number) => (IntType, l.intValue | r.intValue: Number)),
    LongType   -> ((l: Number, r: Number) => (IntType, l.longValue | r.longValue: Number)),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) | toBigInt(r))),
  )

  operation(
    Symbol("xor"),
    IntType    -> ((l: Number, r: Number) => (IntType, l.intValue ^ r.intValue: Number)),
    LongType   -> ((l: Number, r: Number) => (IntType, l.longValue ^ r.longValue: Number)),
    BigIntType -> ((l: Number, r: Number) => maybeDemote(toBigInt(l) ^ toBigInt(r))),
  )

  operation(
    Symbol("compare"),
    IntType      -> ((l: Number, r: Number) => (IntType, l.intValue.compare(r.intValue): Number)),
    LongType     -> ((l: Number, r: Number) => (IntType, l.longValue.compare(r.longValue): Number)),
    BigIntType   -> ((l: Number, r: Number) => (IntType, toBigInt(l).compare(toBigInt(r)): Number)),
    RationalType -> ((l: Number, r: Number) => (IntType, toRational(l).compare(toRational(r)): Number)),
    DoubleType   -> ((l: Number, r: Number) => (IntType, l.doubleValue.compare(r.doubleValue): Number)),
    BigDecType   -> ((l: Number, r: Number) => (IntType, toBigDecimal(l).compare(toBigDecimal(r)): Number)),
  )

}
