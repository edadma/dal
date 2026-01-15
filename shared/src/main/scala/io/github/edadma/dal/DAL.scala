package io.github.edadma.dal

import java.math.{MathContext, RoundingMode}
import java.lang as boxed
import scala.math.*
import io.github.edadma.numbers.{
  BigDecimalMath,
  ComplexBigDecimal,
  ComplexBigInt,
  ComplexDouble,
  ComplexRational,
  QuaternionBigDecimal,
  QuaternionBigInt,
  QuaternionDouble,
  QuaternionRational,
  Rational,
  SmallRational,
}

import scala.collection.mutable

abstract class DAL(implicit var bdmath: BigDecimalMath) {

  protected val opmap    = new mutable.HashMap[(Type, Symbol, Type), Operator]
  protected val specials = new mutable.HashMap[(Type, Type), Type]

  protected def mathContext(p: Int) = new MathContext(p, RoundingMode.HALF_EVEN)

  protected def bisqrt(n: BigInt): Either[BigInt, BigDecimal] = {
    val dr = BigDecimalMath.sqrt(bigDecimal(n))
    val ir = dr.toBigInt

    if (ir * ir == n)
      Left(ir)
    else
      Right(dr)
  }

  def bigDecimal(n: Int): BigDecimal = BigDecimal(n, bdmath.mc)

  def bigDecimal(n: Double): BigDecimal = BigDecimal(n, bdmath.mc)

  def bigDecimal(r: Rational): BigDecimal = r.decimalValue(bdmath.mc)

  def bigDecimal(n: BigInt): BigDecimal = {
    val len = digits(n)
    val m   = if (len >= bdmath.mc.getPrecision) mathContext(len + 5) else bdmath.mc

    BigDecimal(n, m)
  }

  def decimal(s: String): Any = {
    val res = BigDecimal(s, bdmath.mc)

    if (res.isExactDouble) // isValidDouble)
      res.toDouble
    else
      res
  }

  def toSmallRational(a: Number): SmallRational =
    a match {
      case sr: SmallRational            => sr
      case i: boxed.Integer             => SmallRational(i.toLong)
      case l: boxed.Long                => SmallRational(l)
      case bi: BigInt if bi.isValidLong => SmallRational(bi.toLong)
      case r: Rational if r.numerator.isValidLong && r.denominator.isValidLong =>
        SmallRational(r.numerator.toLong, r.denominator.toLong)
      case _ => sys.error("can't convert to SmallRational from " + a)
    }

  def toBigDecimal(a: Number): BigDecimal =
    a match {
      case bd: BigDecimal   => bd
      case i: boxed.Integer => bigDecimal(i)
      case d: boxed.Double  => bigDecimal(d)
      case r: Rational      => bigDecimal(r)
      case bi: BigInt       => bigDecimal(bi)
      case _                => sys.error("can't convert from " + a)
    }

  def toComplexBigInt(a: Number): ComplexBigInt =
    a match {
      case bi: BigInt         => ComplexBigInt(bi)
      case i: boxed.Integer   => ComplexBigInt(toBigInt(i))
      case cbi: ComplexBigInt => cbi
      case _                  => sys.error("can't convert from " + a)
    }

  def toQuaternionBigInt(a: Number): QuaternionBigInt =
    a match {
      case bi: BigInt          => QuaternionBigInt(bi)
      case i: boxed.Integer    => QuaternionBigInt(toBigInt(i))
      case cbi: ComplexBigInt  => QuaternionBigInt(cbi.re, cbi.im, 0, 0)
      case q: QuaternionBigInt => q
      case _                   => sys.error("can't convert from " + a)
    }

  def toComplexRational(a: Number): ComplexRational =
    a match {
      case bi: BigInt          => ComplexRational(toRational(bi))
      case i: boxed.Integer    => ComplexRational(toRational(i))
      case sr: SmallRational   => ComplexRational(sr.toRational)
      case r: Rational         => ComplexRational(r)
      case cbi: ComplexBigInt  => ComplexRational(toRational(cbi.re), toRational(cbi.im))
      case cr: ComplexRational => cr
      case _                   => sys.error("can't convert from " + a)
    }

  def toQuaternionRational(a: Number): QuaternionRational =
    a match {
      case bi: BigInt          => QuaternionRational(toRational(bi))
      case i: boxed.Integer    => QuaternionRational(toRational(i))
      case r: Rational         => QuaternionRational(r)
      case cbi: ComplexBigInt  => QuaternionRational(toRational(cbi.re), toRational(cbi.im), 0, 0)
      case cr: ComplexRational => QuaternionRational(cr.re, cr.im, 0, 0)
      case q: QuaternionBigInt => QuaternionRational(toRational(q.a), toRational(q.b), toRational(q.c), toRational(q.d))
      case _                   => sys.error("can't convert from " + a)
    }

  def toComplexDouble(a: Number): ComplexDouble =
    a match {
      case cd: ComplexDouble   => cd
      case i: boxed.Integer    => ComplexDouble(i.doubleValue)
      case d: boxed.Double     => ComplexDouble(d)
      case sr: SmallRational   => ComplexDouble(sr.doubleValue)
      case r: Rational         => ComplexDouble(r.doubleValue)
      case bi: BigInt          => ComplexDouble(bi.doubleValue)
      case cbi: ComplexBigInt  => ComplexDouble(cbi.re.doubleValue, cbi.im.doubleValue)
      case cr: ComplexRational => ComplexDouble(cr.re.doubleValue, cr.im.doubleValue)
      case _                   => sys.error("can't convert from " + a)
    }

  def toQuaternionDouble(a: Number): QuaternionDouble =
    a match {
      case cd: ComplexDouble   => QuaternionDouble(cd.re, cd.im, 0, 0)
      case q: QuaternionDouble => q
      case i: boxed.Integer    => QuaternionDouble(i.doubleValue)
      case d: boxed.Double     => QuaternionDouble(d)
      case r: Rational         => QuaternionDouble(r.doubleValue)
      case bi: BigInt          => QuaternionDouble(bi.doubleValue)
      case cbi: ComplexBigInt  => QuaternionDouble(cbi.re.doubleValue, cbi.re.doubleValue, 0, 0)
      case q: QuaternionBigInt => QuaternionDouble(q.a.doubleValue, q.b.doubleValue, q.c.doubleValue, q.d.doubleValue)
      case _                   => sys.error("can't convert from " + a)
    }

  def toComplexBigDecimal(a: Number): ComplexBigDecimal =
    a match {
      case cd: ComplexBigDecimal => cd
      case cbi: ComplexBigInt    => ComplexBigDecimal(BigDecimal(cbi.re), BigDecimal(cbi.im))
      case bd: BigDecimal        => new ComplexBigDecimal(bd, 0)
      case i: boxed.Integer      => new ComplexBigDecimal(i.asInstanceOf[Int], 0)
      case d: boxed.Double       => new ComplexBigDecimal(d.asInstanceOf[Double], 0)
      case sr: SmallRational =>
        val rational = sr.toRational
        val quo      = toBigDecimal(rational.numerator) / toBigDecimal(rational.denominator)
        new ComplexBigDecimal(if (quo.precision > bdmath.mc.getPrecision) quo.round(bdmath.mc) else quo, 0)
      case Rational(n, d) =>
        val quo = toBigDecimal(n) / toBigDecimal(d)

        new ComplexBigDecimal(if (quo.precision > bdmath.mc.getPrecision) quo.round(bdmath.mc) else quo, 0)
      case cd: ComplexDouble =>
        new ComplexBigDecimal(BigDecimal(cd.re), BigDecimal(cd.im))
      case cr: ComplexRational =>
        val reQuo    = toBigDecimal(cr.re.numerator) / toBigDecimal(cr.re.denominator)
        val imQuo    = toBigDecimal(cr.im.numerator) / toBigDecimal(cr.im.denominator)
        val reResult = if (reQuo.precision > bdmath.mc.getPrecision) reQuo.round(bdmath.mc) else reQuo
        val imResult = if (imQuo.precision > bdmath.mc.getPrecision) imQuo.round(bdmath.mc) else imQuo
        new ComplexBigDecimal(reResult, imResult)
      case bi: BigInt =>
        val len = digits(bi)

        new ComplexBigDecimal(BigDecimal(bi, if (len >= bdmath.mc.getPrecision) mathContext(len + 5) else bdmath.mc), 0)
      case _ => sys.error("can't convert from " + a)
    }

  def toQuaternionBigDecimal(a: Number): QuaternionBigDecimal =
    a match {
      case q: QuaternionBigDecimal => q
      case q: QuaternionBigInt =>
        QuaternionBigDecimal(BigDecimal(q.a), BigDecimal(q.b), BigDecimal(q.c), BigDecimal(q.d))
      case cbi: ComplexBigInt    => QuaternionBigDecimal(BigDecimal(cbi.re), BigDecimal(cbi.im), 0, 0)
      case cd: ComplexBigDecimal => QuaternionBigDecimal(cd.re, cd.im, 0, 0)
      case bd: BigDecimal        => QuaternionBigDecimal(bd)
      case i: boxed.Integer      => QuaternionBigDecimal(i.asInstanceOf[Int])
      case d: boxed.Double       => QuaternionBigDecimal(d.asInstanceOf[Double])
      case Rational(n, d) =>
        val quo = toBigDecimal(n) / toBigDecimal(d)

        QuaternionBigDecimal(if (quo.precision > bdmath.mc.getPrecision) quo.round(bdmath.mc) else quo)
      case bi: BigInt =>
        val len = digits(bi)

        QuaternionBigDecimal(BigDecimal(bi, if (len >= bdmath.mc.getPrecision) mathContext(len + 5) else bdmath.mc))
      case _ => sys.error("can't convert from " + a)
    }

  protected def intOrDouble(n: Rational): (Type, Number) =
    if (n.isWhole)
      if (n.numerator.isValidInt)
        (IntType, n.numerator.intValue)
      else
        (BigIntType, n.numerator)
    else
      (DoubleType, n.doubleValue)

  protected def bigIntPow(n: Number, e: Number): (Type, Number) =
    e match {
      case i: boxed.Integer =>
        val res = toBigInt(n).pow(abs(i))

        if (i < 0) maybeDemote(Rational.oneOver(res))
        else maybeDemote(res)
      case _ => sys.error(s"exponent too small or too large: $e")
    }

  def maybeDemote(sr: SmallRational): (Type, Number) =
    if (sr.isWhole) {
      if (sr.numerator >= Int.MinValue && sr.numerator <= Int.MaxValue)
        (IntType, sr.numerator.toInt.asInstanceOf[Number])
      else if (sr.numerator >= Long.MinValue && sr.numerator <= Long.MaxValue)
        (LongType, sr.numerator.asInstanceOf[Number])
      else
        (BigIntType, BigInt(sr.numerator))
    } else
      (SmallRationalType, sr)

  protected def safeSmallRationalOp(
      a: Number,
      b: Number,
      safeOp: (SmallRational, SmallRational) => Option[SmallRational],
      fallbackOp: (Rational, Rational) => Rational,
  ): (Type, Number) = {
    val sr1 = toSmallRational(a)
    val sr2 = toSmallRational(b)

    safeOp(sr1, sr2) match {
      case Some(result) => maybeDemote(result)
      case None         =>
        // Promote to Rational on overflow
        val result = fallbackOp(sr1.toRational, sr2.toRational)
        maybeDemote(result)
    }
  }

  def maybeDemote(n: ComplexBigInt): (Type, Number) =
    if (n.im == 0)
      maybeDemote(n.re)
    else
      (ComplexBigIntType, n)

  def maybeDemote(n: QuaternionBigInt): (Type, Number) =
    if (n.c == 0 && n.d == 0) {
      if (n.b == 0)
        maybeDemote(n.a)
      else
        maybeDemote(ComplexBigInt(n.a, n.b))
    } else
      (QuaternionBigIntType, n)

  def maybeDemote(n: ComplexRational): (Type, Number) =
    if (n.im.isZero)
      if (n.re.isWhole)
        if (n.re.numerator.isValidInt)
          (IntType, Integer.valueOf(n.re.numerator.toInt))
        else
          (BigIntType, n.re.numerator)
      else
        (RationalType, n.re)
    else if (n.re.isWhole && n.im.isWhole)
      (ComplexBigIntType, ComplexBigInt(n.re.numerator, n.im.numerator))
    else
      (ComplexRationalType, n)

  def maybeDemote(n: QuaternionRational): (Type, Number) =
    if (n.b.isZero && n.c.isZero && n.d.isZero)
      if (n.a.isWhole)
        if (n.a.numerator.isValidInt)
          (IntType, Integer.valueOf(n.a.numerator.toInt))
        else
          (BigIntType, n.a.numerator)
      else
        (RationalType, n.a)
    else if (n.c.isZero && n.d.isZero)
      maybeDemote(ComplexRational(n.a.numerator, n.b.numerator))
    else if (n.a.isWhole && n.b.isWhole && n.c.isWhole && n.d.isWhole)
      (QuaternionBigIntType, QuaternionBigInt(n.a.numerator, n.b.numerator, n.c.numerator, n.d.numerator))
    else
      (QuaternionRationalType, n)

  protected def boolean(b: Boolean): (Type, Boolean) = (null, b)

//  protected def maybeDemote(n: Double): (Type, Number) =
//    if (n.isValidInt)
//      (IntType, n.toInt.asInstanceOf[Number])
//    else if (n.isWhole)
//      (BigIntType, BigInt(n.toLong))
//    else
//      (DoubleType, n)
//
//  protected def maybeDemote(n: BigDecimal): (Type, Number) =
//    if (n.isValidInt)
//      (IntType, n.toInt.asInstanceOf[Number])
//    else if (n.isWhole)
//      (BigIntType, n.toBigInt)
//    else
//      (BigDecType, n)

  def maybeDemote(n: BigInt): (Type, Number) =
    if (n.isValidInt)
      (IntType, n.toInt.asInstanceOf[Number])
    else
      (BigIntType, n)

  def maybeDemote(r: Rational): (Type, Number) =
    if (r.isWhole) {
      if (r.numerator.isValidInt)
        (IntType, r.intValue.asInstanceOf[Number])
      else if (r.numerator.isValidLong)
        (LongType, r.longValue.asInstanceOf[Number])
      else
        (BigIntType, r.numerator)
    } else {
      if (r.numerator.isValidLong && r.denominator.isValidLong) {
        (SmallRationalType, SmallRational(r.numerator.toLong, r.denominator.toLong))
      } else {
        (RationalType, r)
      }
    }

  protected def maybePromote(n: Long): (Type, Number) =
    if (n.isValidInt)
      (IntType, n.toInt.asInstanceOf[Number])
    else
      (BigIntType, n)

  protected def special(a: Type, b: Type, t: Type): Unit = {
    specials((a, b)) = t
    specials((b, a)) = t
  }

  protected def resultant(rank: mutable.HashMap[Type, Int], l: Type, r: Type): Int =
    specials get (l, r) match {
      case Some(t) => rank(t)
      case None    => rank(l) max rank(r)
    }

  protected def operation(op: Symbol, funcs: (Type, Operator)*): Unit =
    define(op, funcs.asInstanceOf[Seq[(Type, Operator)]])

  protected def relation(op: Symbol, funcs: (Type, (Number, Number) => (Type, Boolean))*): Unit =
    define(op, funcs.asInstanceOf[Seq[(Type, Operator)]])

  protected def define(op: Symbol, funcs: Seq[(Type, Operator)]): Unit = {
    val rank               = new mutable.HashMap[Type, Int]
    val types: Array[Type] = funcs.map(_._1).toArray

    for ((t, r) <- types.zipWithIndex)
      rank(t) = r

    val comb = types.combinations(2).toList map (s => (s.head, s(1)))
    val args = comb ++ comb.map(t => (t._2, t._1)) ++ types.map(t => (t, t))

    for ((l, r) <- args)
      opmap((l, op, r)) = funcs(resultant(rank, l, r))._2
  }

  def compare(x: Number, y: Number): Int = compute(Symbol("compare"), x, y).intValue

  def compute(op: String, left: Number, right: Number): Number = compute(Symbol(op), left, right)

  def compute(op: Symbol, left: Number, right: Number): Number =
    compute(op, DALNumber(left), DALNumber(right), DALNumber.apply).value

  def relate(op: String, left: Number, right: Number): Boolean = relate(Symbol(op), left, right)

  def relate(op: Symbol, left: Number, right: Number): Boolean = relate(op, DALNumber(left), DALNumber(right))

  def relate(op: Symbol, left: TypedNumber, right: TypedNumber): Boolean = {
    val (t, r) = opmap(left.typ, op, right.typ)(left.value, right.value)

    if (t != null)
      sys.error(s"operation '$op' is not a relation")
    else
      r.asInstanceOf[Boolean]
  }

  def compare[T <: TypedNumber](left: TypedNumber, right: TypedNumber): Int = {
    val (_, n) = opmap(left.typ, Symbol("compare"), right.typ)(left.value, right.value)

    n.asInstanceOf[Number].intValue
  }

  def compute[T <: TypedNumber](op: Symbol, left: TypedNumber, right: TypedNumber, number: ((Type, Number)) => T): T = {
    val n @ (t, _) = opmap(left.typ, op, right.typ)(left.value, right.value)

    if (t == null)
      sys.error(s"operation '$op' does not return number")
    else
      number(n.asInstanceOf[(Type, Number)])
  }

  def perform(op: Symbol, left: Number, right: Number): Any =
    opmap(numberType(left), op, numberType(right))(left, right)._2

  def perform[T](
      op: Symbol,
      left: TypedNumber,
      right: TypedNumber,
      number: ((Type, Number)) => T,
      boolean: Boolean => T,
  ): T =
    opmap(left.typ, op, right.typ)(left.value, right.value) match {
      case (null, b: java.lang.Boolean) => boolean(b)
      case n                            => number(n.asInstanceOf[(Type, Number)])
    }

  def negate(typ: Type, n: Number): (Type, Number) =
    (typ, n) match {
      case (IntType, a: boxed.Integer)           => (IntType, -a)
      case (LongType, a: boxed.Long)             => (LongType, -a)
      case (BigIntType, a: BigInt)               => (BigIntType, -a)
      case (SmallRationalType, a: SmallRational) => (SmallRationalType, -a)
      case (RationalType, a: Rational)           => (RationalType, -a)
      case (DoubleType, a: boxed.Double)         => (DoubleType, -a)
      case (BigDecType, a: BigDecimal)           => (BigDecType, -a)
      case (_, _)                                => ???
    }

  def negate[T <: TypedNumber](n: TypedNumber, number: ((Type, Number)) => T): T =
    number(negate(n.typ, n.value))

  def negate(n: Number): Number =
    n match {
      case a: boxed.Integer => -a
      case a: boxed.Long    => -a
      case a: BigInt        => -a
      case a: Rational      => -a
      case a: boxed.Double  => -a
      case a: BigDecimal    => -a
      case a: ComplexBigInt => -a
    }

  def invert(n: Number): (Type, Number) =
    n match {
      case a: boxed.Integer => maybePromote(~a & 0xffffffffL)
      case a: BigInt        => maybeDemote(~a)
    }

  def sqrtFunction(n: Any): Number =
    n match {
      case a: boxed.Integer =>
        val n  = abs(a)
        val dr = sqrt(n.toDouble)
        val ir = round(dr).toInt

        if (ir * ir == n)
          if (a < 0) new ComplexBigInt(0, ir) else ir
        else if (a < 0) new ComplexDouble(0, dr)
        else dr
      case a: BigInt =>
        bisqrt(a.abs) match {
          case Left(ir)  => if (a < 0) new ComplexBigInt(0, ir) else maybeDemote(ir)._2
          case Right(dr) => if (a < 0) new ComplexDouble(0, dr.doubleValue) else dr
        }
      case a: Rational =>
        val ar = a.abs

        def rsqrt = if (a < 0) new ComplexDouble(0, sqrt(ar.doubleValue)) else sqrt(ar.doubleValue).asInstanceOf[Number]

        bisqrt(ar.numerator) match {
          case Left(irn) =>
            bisqrt(ar.denominator) match {
              case Left(ird) =>
                val res = Rational(irn, ird)

                if (a < 0) new ComplexRational(0, res) else res
              case _ => rsqrt
            }
          case _ => rsqrt
        }
      case a: boxed.Double  => if (a < 0) new ComplexDouble(0, sqrt(-a)) else sqrt(a)
      case a: BigDecimal    => if (a < 0) new ComplexBigDecimal(0, BigDecimalMath.sqrt(-a)) else BigDecimalMath.sqrt(a)
      case a: ComplexBigInt => a.sqrt
      case a: ComplexRational   => a.sqrt
      case a: ComplexDouble     => a.sqrt
      case a: ComplexBigDecimal => a.sqrt
    }

  def absFunction(n: Any): Number =
    n match {
      case a: boxed.Integer                     => maybePromote(abs(a.longValue))._2
      case a: BigInt                            => maybeDemote(a.abs)._2
      case a: io.github.edadma.numbers.Rational => a.abs
      case a: boxed.Double                      => abs(a)
      case a: BigDecimal                        => a.abs
      case a: ComplexBigInt                     => a.abs
      case a: ComplexRational                   => a.abs
      case a: ComplexDouble                     => a.abs
      case a: ComplexBigDecimal                 => a.abs
    }

  def lnFunction(n: Any): Number =
    n match {
      case a: boxed.Integer                     => log(a.doubleValue)
      case a: BigInt                            => log(a.doubleValue)
      case a: io.github.edadma.numbers.Rational => log(a.doubleValue)
      case a: boxed.Double                      => log(a)
      case a: BigDecimal                        => BigDecimalMath.ln(a)
      case a: ComplexBigInt                     => a.ln
      case a: ComplexRational                   => a.ln
      case a: ComplexDouble                     => a.ln
      case a: ComplexBigDecimal                 => a.ln
    }

  def floorFunction(n: Any): Number =
    n match {
      case a: boxed.Integer                     => a
      case a: BigInt                            => maybeDemote(a)._2
      case a: io.github.edadma.numbers.Rational => a.floor
      case a: boxed.Double =>
        val f = BigDecimal(a).setScale(0, BigDecimal.RoundingMode.FLOOR)

        if (f.isValidInt)
          f.toIntExact
        else
          f.toBigInt
      case a: BigDecimal        => a.setScale(0, BigDecimal.RoundingMode.FLOOR)
      case a: ComplexBigInt     => a
      case a: ComplexRational   => a.floor
      case a: ComplexDouble     => a.floor
      case a: ComplexBigDecimal => a.floor
    }

  def ceilFunction(n: Any): Number =
    n match {
      case a: boxed.Integer                     => a
      case a: BigInt                            => maybeDemote(a)._2
      case a: io.github.edadma.numbers.Rational => a.ceil
      case a: boxed.Double =>
        val f = BigDecimal(a).setScale(0, BigDecimal.RoundingMode.CEILING)

        if (f.isValidInt)
          f.toIntExact
        else
          f.toBigInt
      case a: BigDecimal        => a.setScale(0, BigDecimal.RoundingMode.CEILING)
      case a: ComplexBigInt     => a
      case a: ComplexRational   => a.ceil
      case a: ComplexDouble     => a.ceil
      case a: ComplexBigDecimal => a.ceil
    }

  def roundFunction(n: Any): Number =
    n match {
      case a: boxed.Integer => a
      case a: BigInt        => maybeDemote(a)._2
      case a: io.github.edadma.numbers.Rational =>
        val r = BigDecimal(a.numerator) / BigDecimal(a.denominator)
        val rounded = r.setScale(0, BigDecimal.RoundingMode.HALF_UP)
        if (rounded.isValidInt) rounded.toIntExact
        else rounded.toBigInt
      case a: boxed.Double =>
        val r = BigDecimal(a).setScale(0, BigDecimal.RoundingMode.HALF_UP)
        if (r.isValidInt) r.toIntExact
        else r.toBigInt
      case a: BigDecimal =>
        a.setScale(0, BigDecimal.RoundingMode.HALF_UP)
      case a: ComplexBigInt => a
      case a: ComplexRational =>
        val reRounded = BigDecimal(a.re.numerator) / BigDecimal(a.re.denominator)
        val imRounded = BigDecimal(a.im.numerator) / BigDecimal(a.im.denominator)
        val re = reRounded.setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
        val im = imRounded.setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
        maybeDemote(ComplexBigInt(re, im))._2
      case a: ComplexDouble =>
        val re = BigDecimal(a.re).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
        val im = BigDecimal(a.im).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
        maybeDemote(ComplexBigInt(re, im))._2
      case a: ComplexBigDecimal =>
        ComplexBigDecimal(
          a.re.setScale(0, BigDecimal.RoundingMode.HALF_UP),
          a.im.setScale(0, BigDecimal.RoundingMode.HALF_UP),
        )
    }

  def cosFunction(n: Any): Number =
    n match {
      case _: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double =>
        cos(n.asInstanceOf[Number].doubleValue).asInstanceOf[boxed.Double]
      case a: BigDecimal        => BigDecimalMath.cos(a)
      case a: ComplexBigInt     => a.cos
      case a: ComplexRational   => a.cos
      case a: ComplexDouble     => a.cos
      case a: ComplexBigDecimal => a.cos
    }

  def sinFunction(n: Any): Number =
    n match {
      case _: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double =>
        sin(n.asInstanceOf[Number].doubleValue).asInstanceOf[boxed.Double]
      case a: BigDecimal        => BigDecimalMath.sin(a)
      case a: ComplexBigInt     => a.sin
      case a: ComplexRational   => a.sin
      case a: ComplexDouble     => a.sin
      case a: ComplexBigDecimal => a.sin
    }

  def tanFunction(n: Any): Number =
    n match {
      case _: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double =>
        tan(n.asInstanceOf[Number].doubleValue).asInstanceOf[boxed.Double]
      //			case a: BigDecimal => BigDecimalMath.tan( a )
      case a: ComplexBigInt     => a.tan
      case a: ComplexRational   => a.tan
      case a: ComplexDouble     => a.tan
      case a: ComplexBigDecimal => a.tan
    }

  def acosFunction(n: Any): Number =
    n match {
      case _: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double =>
        acos(n.asInstanceOf[Number].doubleValue).asInstanceOf[boxed.Double]
      case a: BigDecimal        => BigDecimalMath.acos(a)
      case a: ComplexBigInt     => a.acos
      case a: ComplexRational   => a.acos
      case a: ComplexDouble     => a.acos
      case a: ComplexBigDecimal => a.acos
    }

  def asinFunction(n: Any): Number =
    n match {
      case _: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double =>
        asin(n.asInstanceOf[Number].doubleValue).asInstanceOf[boxed.Double]
      case a: BigDecimal        => BigDecimalMath.asin(a)
      case a: ComplexBigInt     => a.asin
      case a: ComplexRational   => a.asin
      case a: ComplexDouble     => a.asin
      case a: ComplexBigDecimal => a.asin
    }

  def atanFunction(n: Any): Number =
    n match {
      case _: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double =>
        atan(n.asInstanceOf[Number].doubleValue).asInstanceOf[boxed.Double]
      case a: BigDecimal        => BigDecimalMath.atan(a)
      case a: ComplexBigInt     => a.atan
      case a: ComplexRational   => a.atan
      case a: ComplexDouble     => a.atan
      case a: ComplexBigDecimal => a.atan
    }

  def expFunction(n: Any): Number =
    n match {
      case _: boxed.Integer | _: BigInt | _: Rational | _: boxed.Double =>
        exp(n.asInstanceOf[Number].doubleValue).asInstanceOf[boxed.Double]
      case a: BigDecimal        => BigDecimalMath.exp(a)
      case a: ComplexBigInt     => a.exp
      case a: ComplexRational   => a.exp
      case a: ComplexDouble     => a.exp
      case a: ComplexBigDecimal => a.exp
    }

}

case class DALNumber(typ: Type, value: Number) extends TypedNumber

object DALNumber {

  def apply(n: (Type, Number)): DALNumber = DALNumber(n._1, n._2)

  def apply(n: Int) = new DALNumber(IntType, n)

  def apply(n: Long) = new DALNumber(LongType, n)

  def apply(n: BigInt) = new DALNumber(BigIntType, n)

  def apply(n: Rational) = new DALNumber(RationalType, n)

  def apply(n: Double) = new DALNumber(DoubleType, n)

  def apply(n: BigDecimal) = new DALNumber(BigDecType, n)

  def apply(n: Number): DALNumber = new DALNumber(numberType(n), n)

}

trait TypedNumber {
  val typ: Type
  val value: Number
}

abstract class Type
case object IntType                extends Type
case object LongType               extends Type
case object BigIntType             extends Type
case object SmallRationalType      extends Type
case object RationalType           extends Type
case object DoubleType             extends Type
case object BigDecType             extends Type
case object ComplexIntType         extends Type
case object ComplexBigIntType      extends Type
case object ComplexRationalType    extends Type
case object ComplexDoubleType      extends Type
case object ComplexBigDecType      extends Type
case object QuaternionIntType      extends Type
case object QuaternionBigIntType   extends Type
case object QuaternionRationalType extends Type
case object QuaternionDoubleType   extends Type
case object QuaternionBigDecType   extends Type
