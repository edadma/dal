# DAL: Dynamic Arithmetic Logic

![Scala Version](https://img.shields.io/badge/Scala-3.5.2-red.svg)
![Cross Platform](https://img.shields.io/badge/Platform-JVM%20%7C%20JS%20%7C%20Native-blue.svg)
![License](https://img.shields.io/badge/License-ISC-green.svg)

A **sophisticated runtime number system** for building interpreters, dynamic languages, and computer algebra systems that need **automatic type promotion** and **exact arithmetic** with optimal performance.

## Overview

DAL (Dynamic Arithmetic Logic) provides **automatic number type promotion** and **runtime arithmetic operations** that adapt to the precision requirements of your calculations. It seamlessly handles operations between different number types and automatically promotes to higher precision when needed.

### Key Features

- 🚀 **Automatic Type Promotion**: Operations automatically promote to higher precision types when needed
- 🎯 **Exact Arithmetic**: Maintains mathematical precision through rational and arbitrary precision types
- ⚡ **Performance Optimized**: Uses the most efficient representation for each value
- 🌐 **Cross-Platform**: Works on JVM, JavaScript, and Native platforms
- 🔧 **Runtime Flexible**: Perfect for interpreters, calculators, and dynamic languages
- 📐 **Mathematical Types**: Supports complex numbers, quaternions, and arbitrary precision

## Type Hierarchy & Automatic Promotion

DAL manages a sophisticated type hierarchy with automatic promotion:

```
Int ──→ Long ──→ BigInt ──→ Double ──→ BigDecimal
 │        │        │                      ↑
 ↓        ↓        ↓                      │
 └──→ Rational ───┘──────────────────────┘
      │
      ├──→ ComplexRational ──→ ComplexBigDecimal
      │     │
      │     └──→ ComplexDouble
      │
      └──→ QuaternionRational ──→ QuaternionBigDecimal
            │
            └──→ QuaternionDouble
```

### Smart Promotion Logic

```scala
1 + 2           // → 3 (Int)
1 + 2.5         // → 3.5 (Double)  
1 / 3           // → 1/3 (Rational) - Exact!
1.5 + 2/3       // → 2.1666... (Double)
BigInt("999") * BigInt("999")  // → BigInt (as needed)
```

## Quick Start

### Basic Usage

```scala
import io.github.edadma.dal._

// Use PrecisionDAL for exact arithmetic
val result1 = PrecisionDAL.compute("+", 1, 2)           // 3 (Int)
val result2 = PrecisionDAL.compute("/", 1, 3)           // 1/3 (Rational)
val result3 = PrecisionDAL.compute("+", result2, result2) // 2/3 (Rational)

// Automatic promotion on overflow
val big1 = PrecisionDAL.compute("*", Long.MaxValue, 2)  // → BigInt
val exact = PrecisionDAL.compute("/", 22, 7)            // → 22/7 (Rational)

// Boolean operations
val comparison = PrecisionDAL.relate("=", Rational(1,2), 0.5)  // true
val ordering = PrecisionDAL.relate("<", 1, 2)                 // true
```

### For Interpreter Development

```scala
import io.github.edadma.dal._

class Calculator {
  def evaluate(left: Any, operator: String, right: Any): Number = {
    val leftNum = convertToNumber(left)
    val rightNum = convertToNumber(right)
    
    // DAL handles all type conversions and promotions automatically
    PrecisionDAL.compute(Symbol(operator), leftNum, rightNum)
  }
  
  def convertToNumber(value: Any): Number = value match {
    case i: Int => i
    case d: Double => d
    case s: String if s.contains('/') => Rational(s)  // "1/3" → Rational
    case s: String => BigDecimal(s)                   // High precision
    case n: Number => n
  }
}

val calc = Calculator()
calc.evaluate("1/3", "+", "1/6")     // → 1/2 (Rational)
calc.evaluate(Long.MaxValue, "*", 2) // → BigInt (auto-promotion)
calc.evaluate("0.1", "+", "0.2")     // → 0.3 (BigDecimal, exact)
```

## DAL Implementations

DAL provides different implementations optimized for different use cases:

### BasicDAL
**Fast arithmetic with lossy division**
- Division of integers returns integers or doubles
- Optimized for performance
- Good for numerical computing

```scala
BasicDAL.compute("/", 1, 3)  // → 0.333... (Double)
BasicDAL.compute("+", 1, 2)  // → 3 (Int)
```

### PrecisionDAL
**Exact arithmetic with rational numbers**
- Division of integers creates exact rational numbers
- Maintains mathematical precision
- Perfect for computer algebra systems

```scala
PrecisionDAL.compute("/", 1, 3)  // → 1/3 (Rational) - Exact!
PrecisionDAL.compute("^", Rational(2,3), 2)  // → 4/9 (Rational)
```

### ComplexDAL
**Complex number support with exact arithmetic**
- Includes complex numbers in the type hierarchy
- Exact complex rational arithmetic
- For mathematical and scientific computing

```scala
ComplexDAL.compute("+", ComplexDouble(1,2), ComplexDouble(3,4))  // → 4+6i
ComplexDAL.compute("*", Rational(1,2), ComplexRational(1,1,2,1)) // Mixed types work!
```

### QuaternionDAL
**Full quaternion support**
- 4D number system for 3D rotations
- Includes all complex number capabilities
- For graphics, robotics, and advanced mathematics

```scala
QuaternionDAL.compute("*", QuaternionDouble.i, QuaternionDouble.j)  // → k
QuaternionDAL.compute("+", QuaternionRational(1,0,0,0), QuaternionInt(0,1,0,0))
```

## Advanced Features

### Custom Precision Control

```scala
import io.github.edadma.numbers.BigDecimalMath

// Use custom precision context
implicit val customPrecision = new BigDecimalMath(50)  // 50 decimal places

val result = PrecisionDAL.compute("/", BigDecimal("1"), BigDecimal("3"))
// → 0.33333333333333333333333333333333333333333333333333 (50 digits)
```

### Functional Programming Integration

```scala
import io.github.edadma.dal.PrecisionNumberIsFractional._

// Works with Scala collections automatically
val numbers = List(1, Rational(1,2), BigDecimal("0.25"))
val sum = numbers.sum  // → 1.75 (BigDecimal)

val rationals = List(Rational(1,2), Rational(1,3), Rational(1,6))  
val total = rationals.sum  // → 1 (Rational) - Exact!
```

### Error Handling & Overflow

```scala
// Automatic promotion prevents overflow
val big = PrecisionDAL.compute("*", Int.MaxValue, Int.MaxValue)  // → BigInt

// Division by zero is handled gracefully
try {
  PrecisionDAL.compute("/", 1, 0)
} catch {
  case e: RuntimeException => println("Division by zero!")
}

// Complex operations maintain precision
val complex = PrecisionDAL.compute("/", ComplexRational(1,1,1,1), ComplexRational(2,1,0,0))
// → Exact complex rational result
```

## Performance Characteristics

### Operation Speed by Type

| Type | Relative Speed | Memory Usage | Use Case |
|------|---------------|--------------|----------|
| `Int` | 1x (baseline) | 4 bytes | Small integers |
| `Long` | 1x | 8 bytes | Large integers |
| `Double` | 1x | 8 bytes | Approximate decimals |
| `BigInt` | 10-50x slower | 24+ bytes | Arbitrary integers |
| `Rational` | 20-100x slower | 48+ bytes | Exact fractions |
| `BigDecimal` | 50-200x slower | 40+ bytes | Arbitrary precision |

### Smart Type Selection

DAL automatically chooses the most efficient type:

```scala
// Stays in fast types when possible
PrecisionDAL.compute("+", 1, 2)                    // → Int (fastest)
PrecisionDAL.compute("*", 1000, 1000)              // → Int (fits)
PrecisionDAL.compute("*", 100000, 100000)          // → Long (promoted)
PrecisionDAL.compute("*", Long.MaxValue, 2)        // → BigInt (necessary)

// Uses exact types for exact operations  
PrecisionDAL.compute("/", 1, 3)                    // → Rational (exact)
PrecisionDAL.compute("+", Rational(1,3), Rational(1,6)) // → Rational(1,2)
```

## Integration Examples

### Building a Calculator REPL

```scala
import io.github.edadma.dal._

object Calculator extends App {
  val operators = Set("+", "-", "*", "/", "^", "=", "<", ">")
  
  def evaluateExpression(input: String): String = {
    val tokens = input.split("\\s+")
    if (tokens.length != 3) return "Error: Expected 'number operator number'"
    
    val left = parseNumber(tokens(0))
    val op = tokens(1)
    val right = parseNumber(tokens(2))
    
    if (operators.contains(op)) {
      if (Set("=", "<", ">").contains(op)) {
        PrecisionDAL.relate(Symbol(op), left, right).toString
      } else {
        PrecisionDAL.compute(Symbol(op), left, right).toString
      }
    } else {
      "Error: Unknown operator"
    }
  }
  
  def parseNumber(s: String): Number = {
    if (s.contains('/')) Rational(s)
    else if (s.contains('.')) BigDecimal(s)
    else if (s.length > 9) BigInt(s)
    else s.toInt
  }
  
  // REPL loop
  println("DAL Calculator - Enter expressions like '1/3 + 1/6'")
  var continue = true
  while (continue) {
    print("> ")
    val input = scala.io.StdIn.readLine()
    if (input == "quit") {
      continue = false
    } else {
      println(evaluateExpression(input))
    }
  }
}

// Example session:
// > 1/3 + 1/6
// 1/2
// > 1000000000000 * 1000000000000  
// 1000000000000000000000000
// > 1 / 3
// 1/3
// > 0.1 + 0.2
// 0.3
```

### Embedding in a Programming Language

```scala
import io.github.edadma.dal._

trait Value
case class NumberValue(n: Number) extends Value
case class BooleanValue(b: Boolean) extends Value

class Interpreter {
  def evaluateBinaryOp(left: Value, op: String, right: Value): Value = {
    (left, right) match {
      case (NumberValue(l), NumberValue(r)) =>
        if (Set("=", "!=", "<", ">", "<=", ">=").contains(op)) {
          BooleanValue(PrecisionDAL.relate(Symbol(op), l, r))
        } else {
          NumberValue(PrecisionDAL.compute(Symbol(op), l, r))
        }
      case _ => throw new RuntimeException("Type error")
    }
  }
  
  // Automatic type promotion in action
  def demo(): Unit = {
    val a = NumberValue(1)
    val b = NumberValue(Rational(1, 3))
    
    val sum = evaluateBinaryOp(a, "+", b)  // → NumberValue(Rational(4,3))
    val product = evaluateBinaryOp(a, "*", b)  // → NumberValue(Rational(1,3))
    val comparison = evaluateBinaryOp(a, ">", b)  // → BooleanValue(true)
  }
}
```

## API Reference

### Core Operations

```scala
// Arithmetic operations
def compute(op: Symbol, left: Number, right: Number): Number
def compute(op: String, left: Number, right: Number): Number

// Comparison operations  
def relate(op: Symbol, left: Number, right: Number): Boolean
def relate(op: String, left: Number, right: Number): Boolean

// Unary operations
def negate(n: Number): Number
```

### Supported Operations

**Arithmetic**: `+`, `-`, `*`, `/`, `//` (float division), `^` (power), `mod`, `\` (integer division)

**Comparison**: `=`, `!=`, `<`, `>`, `<=`, `>=`

**Bitwise** (integer types): `and`, `or`, `xor`

**Special**: `div` (divisibility test), `compare` (three-way comparison)

### Type Conversion Methods

```scala
def toBigInt(a: Number): BigInt
def toRational(a: Number): Rational  
def toBigDecimal(a: Number): BigDecimal
def numberType(n: Number): Type
def maybeDemote(n: Number): (Type, Number)  // Optimize representation
```

### Mathematical Functions

```scala
def sqrtFunction(n: Any): Number      // Square root with complex results
def absFunction(n: Any): Number       // Absolute value
def floorFunction(n: Any): Number     // Floor function
def ceilFunction(n: Any): Number      // Ceiling function
def expFunction(n: Any): Number       // Exponential
def lnFunction(n: Any): Number        // Natural logarithm
def sinFunction(n: Any): Number       // Sine
def cosFunction(n: Any): Number       // Cosine
def tanFunction(n: Any): Number       // Tangent
// ... and more trigonometric functions
```

## Dependencies

DAL is built on the [numbers](https://github.com/edadma/numbers) library which provides:
- `Rational`: Exact rational arithmetic with BigInt numerator/denominator
- `ComplexRational`, `ComplexDouble`, `ComplexBigDecimal`: Complex number types
- `QuaternionRational`, `QuaternionDouble`, `QuaternionBigDecimal`: Quaternion types
- `BigDecimalMath`: High-precision mathematical functions

## Building and Installation

### SBT

```scala
libraryDependencies += "io.github.edadma" %%% "dal" % "0.1.10"
```

### Development

```bash
git clone https://github.com/edadma/dal.git
cd dal
sbt compile
sbt test
```

### Cross-Platform Build

```bash
sbt "project dalJVM" compile
sbt "project dalJS" compile  
sbt "project dalNative" compile
```

## Use Cases

### Perfect For

- 🔧 **Programming Language Interpreters**: Automatic type promotion for dynamic languages
- 🧮 **Computer Algebra Systems**: Exact symbolic computation with rationals
- 🎯 **Calculators**: High-precision arithmetic with automatic precision selection
- 🔬 **Scientific Computing**: Complex and quaternion arithmetic with exact rationals
- 💰 **Financial Systems**: Exact decimal arithmetic without floating-point errors
- 📊 **Data Analysis**: Mixed-precision calculations with automatic optimization

### Examples in the Wild

```scala
// Scheme/Lisp interpreter numeric tower
def evaluateNumber(expr: Any): Number = expr match {
  case i: Int => i
  case s: String if s.contains('/') => Rational(s)
  case s: String => BigDecimal(s)
}

// Computer algebra system  
def simplify(expr: Expr): Expr = expr match {
  case Add(Number(a), Number(b)) => Number(PrecisionDAL.compute("+", a, b))
  case Multiply(Number(a), Number(b)) => Number(PrecisionDAL.compute("*", a, b))
  // ... symbolic rules
}

// Financial calculations
def calculateCompoundInterest(principal: String, rate: String, time: Int): BigDecimal = {
  val p = BigDecimal(principal)
  val r = Rational(rate)  // e.g., "5/100" for 5%
  val amount = PrecisionDAL.compute("*", p, PrecisionDAL.compute("^", 
    PrecisionDAL.compute("+", 1, r), time))
  amount.asInstanceOf[BigDecimal]  // Exact result
}
```

## Performance Tips

1. **Choose the Right DAL**: Use `BasicDAL` for speed, `PrecisionDAL` for exactness
2. **Input Type Matters**: Start with the most efficient type (`Int` vs `BigDecimal`)
3. **Batch Operations**: Multiple operations on same types are faster
4. **Avoid Unnecessary Precision**: Don't use `BigDecimal` when `Double` suffices

## Contributing

We welcome contributions! The DAL library is designed to be:
- **Extensible**: Easy to add new number types and operations
- **Testable**: Comprehensive test coverage for all type combinations
- **Portable**: Works across JVM, JavaScript, and Native platforms

## License

This project is licensed under the ISC License - see the [LICENSE](LICENSE) file for details.

## Related Projects

- [numbers](https://github.com/edadma/numbers) - The underlying exact arithmetic library
- [spire](https://github.com/typelevel/spire) - Advanced numeric abstractions for Scala
- [breeze](https://github.com/scalanlp/breeze) - Numerical processing library