package io.github.edadma.dal

import io.github.edadma.numbers.ComplexBigInt

def parseNumber(s: String): Number = {
  import io.github.edadma.numbers._

  s.trim match {
    // Quaternions like "1+2i+3j+4k" - check first since they contain 'i'
    case quat if quat.contains('j') || quat.contains('k') =>
      QuaternionDoubleIsFractional.quaternionDoubleIsFractional.parseString(quat) match {
        case Some(q) => q
        case None    => throw new IllegalArgumentException(s"Invalid quaternion: $s")
      }

    // Complex numbers like "3+4i", "2-5i", "3+i", "-2i"
    case complex if complex.endsWith("i") && !complex.endsWith("ki") =>
      // First try ComplexRational for exact rational coefficients like "1/2+1/3i"
      ComplexRationalIsFractional.complexRationalIsFractional.parseString(complex) match {
        case Some(c) => c
        case None    =>
          // Second try ComplexBigInt for exact integer coefficients like "3+4i"
          parseAsComplexBigInt(complex) match {
            case Some(c) => c
            case None    =>
              // Fallback to ComplexDouble for decimal coefficients like "3.14+2.71i"
              ComplexDoubleIsFractional.complexDoubleIsFractional.parseString(complex) match {
                case Some(c) => c
                case None    => throw new IllegalArgumentException(s"Invalid complex number: $s")
              }
          }
      }

    // Rational numbers like "3/4"
    case rational if rational.contains('/') && !rational.contains('.') =>
      try {
        // Try SmallRational first for efficiency
        SmallRational(rational)
      } catch {
        case _: NumberFormatException =>
          // Fallback to big Rational on overflow
          Rational(rational)
      }

    // Decimal numbers
    case decimal if decimal.contains('.') || decimal.toLowerCase.contains('e') =>
      try {
        decimal.toDouble
      } catch {
        case _: NumberFormatException => throw new IllegalArgumentException(s"Invalid decimal: $s")
      }

    // Integer numbers (promote through the type hierarchy)
    case integer =>
      try {
        BigInt(integer) match {
          case n if n.isValidInt  => n.toInt.asInstanceOf[Number]
          case n if n.isValidLong => n.toLong.asInstanceOf[Number]
          case n                  => n
        }
      } catch {
        case _: NumberFormatException => throw new IllegalArgumentException(s"Invalid integer: $s")
      }
  }
}

// Helper function to parse integer complex numbers as ComplexBigInt
def parseAsComplexBigInt(s: String): Option[ComplexBigInt] = {
  import io.github.edadma.numbers.ComplexBigInt

  // Remove whitespace
  val trimmed = s.trim

  try {
    // Handle pure imaginary cases first: "4i", "-4i", "i", "-i"
    if (trimmed.matches("""^[+-]?\d*i$""")) {
      val coefficient = trimmed.dropRight(1) // Remove 'i'
      val imagPart = coefficient match {
        case "" | "+" => BigInt(1)
        case "-"      => BigInt(-1)
        case c        => BigInt(c)
      }
      return Some(ComplexBigInt(BigInt(0), imagPart))
    }

    // Handle full complex numbers: "3+4i", "5-7i", "2+0i", etc.
    // Pattern: optional_minus + digits + (+ or -) + optional_digits + i
    val fullComplexPattern = """^(-?\d+)([+-])(\d*)i$""".r

    trimmed match {
      case fullComplexPattern(re, sign, im) =>
        val realPart  = BigInt(re)
        val imagCoeff = if (im.isEmpty) BigInt(1) else BigInt(im)
        val imagPart  = if (sign == "-") -imagCoeff else imagCoeff
        Some(ComplexBigInt(realPart, imagPart))
      case _ => None
    }
  } catch {
    case _: NumberFormatException => None
  }
}

def eval(expression: String, dal: DAL): Any = {
  val toks = expression.split(' ')
  require(toks.length == 3, s"Expected 3 tokens, got ${toks.length}")

  val left  = parseNumber(toks(0))
  val op    = toks(1)
  val right = parseNumber(toks(2))

  op match {
    // Arithmetic operations
    case "+" | "-" | "*" | "/" | "%" | "^" | "//" | "mod" | "\\" | "and" | "or" | "xor" =>
      dal.compute(Symbol(op), left, right)
    // Comparison operations
    case "=" | "!=" | "<" | ">" | "<=" | ">=" | "div" =>
      dal.relate(Symbol(op), left, right)
    // Three-way comparison returns a number
    case "compare" =>
      dal.compute(Symbol(op), left, right)
    case _ =>
      throw new IllegalArgumentException(s"Unknown operator: $op")
  }
}
