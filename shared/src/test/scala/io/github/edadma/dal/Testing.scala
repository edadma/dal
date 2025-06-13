package io.github.edadma.dal

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
      // Try ComplexDouble first for decimal complex numbers
      ComplexDoubleIsFractional.complexDoubleIsFractional.parseString(complex) match {
        case Some(c) => c
        case None    =>
          // Try ComplexRational for rational complex numbers like "1/2+1/3i"
          ComplexRationalIsFractional.complexRationalIsFractional.parseString(complex) match {
            case Some(c) => c
            case None    => throw new IllegalArgumentException(s"Invalid complex number: $s")
          }
      }

    // Rational numbers like "3/4"
    case rational if rational.contains('/') && !rational.contains('.') =>
      try {
        Rational(rational)
      } catch {
        case _: Exception => throw new IllegalArgumentException(s"Invalid rational: $s")
      }

    // Decimal numbers
    case decimal if decimal.contains('.') =>
      try {
        decimal.toDouble
      } catch {
        case _: NumberFormatException => throw new IllegalArgumentException(s"Invalid decimal: $s")
      }

    // Integer numbers (promote to BigInt if needed)
    case integer =>
      try {
        BigInt(integer) match {
          case n if n.isValidInt => n.toInt.asInstanceOf[Number]
          case n                 => n
        }
      } catch {
        case _: NumberFormatException => throw new IllegalArgumentException(s"Invalid integer: $s")
      }
  }
}

def eval(expression: String): Any = {
  val toks = expression.split(' ')

  if (toks.length != 3) {
    throw new IllegalArgumentException(s"Expected 3 tokens, got ${toks.length}: ${toks.mkString("[", ", ", "]")}")
  }

  val left  = parseNumber(toks(0))
  val op    = toks(1)
  val right = parseNumber(toks(2))

  op match {
    // Arithmetic operations
    case "+" | "-" | "*" | "/" | "%" | "^" =>
      PrecisionDAL.compute(Symbol(op), left, right)

    // Relational operations
    case "=" | "!=" | "<" | ">" | "<=" | ">=" | "div" =>
      PrecisionDAL.relate(Symbol(op), left, right)

    case _ =>
      throw new IllegalArgumentException(s"Unknown operator: $op")
  }
}

def evalArithmetic(expression: String): Number = {
  eval(expression) match {
    case n: Number => n
    case other     => throw new IllegalArgumentException(s"Expected Number, got: $other")
  }
}

def evalRelation(expression: String): Boolean = {
  eval(expression) match {
    case b: Boolean => b
    case other      => throw new IllegalArgumentException(s"Expected Boolean, got: $other")
  }
}
