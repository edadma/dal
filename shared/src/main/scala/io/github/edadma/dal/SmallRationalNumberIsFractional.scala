package io.github.edadma.dal

import io.github.edadma.numbers.SmallRational

trait SmallRationalNumberIsFractional extends Fractional[Number] {

  def plus(x: Number, y: Number): Number = PrecisionDAL.compute(Symbol("+"), x, y)

  def minus(x: Number, y: Number): Number = PrecisionDAL.compute(Symbol("-"), x, y)

  def times(x: Number, y: Number): Number = PrecisionDAL.compute(Symbol("*"), x, y)

  def div(x: Number, y: Number): Number = PrecisionDAL.compute(Symbol("/"), x, y)

  def negate(x: Number): Number = PrecisionDAL.negate(x)

  def parseString(str: String): Option[Number] = {
    try {
      // Try SmallRational first, then fall back to other types
      if (str.contains('/')) {
        Some(SmallRational(str))
      } else {
        Some(Integer.decode(str))
      }
    } catch {
      case _: Exception => None
    }
  }

  def fromInt(x: Int): Number = x

  def toInt(x: Number): Int = x.intValue

  def toLong(x: Number): Long = x.longValue

  def toFloat(x: Number): Float = x.floatValue

  def toDouble(x: Number): Double = x.doubleValue

  def compare(x: Number, y: Number): Int = PrecisionDAL.compare(x, y)
}

object SmallRationalNumberIsFractional {
  implicit object numberIsFractional extends SmallRationalNumberIsFractional
}
