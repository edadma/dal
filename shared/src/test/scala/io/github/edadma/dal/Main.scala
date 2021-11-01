package io.github.edadma.dal

import io.github.edadma.numbers.Rational

import scala.language.postfixOps

object Main extends App {

  val a = Rational(1, 4)
  val b = PrecisionDAL.compute("/", 3, 4)

  println(PrecisionDAL.compute("+", a, b))

  val tokens = "-?[0-9.]+|[-+*/%^=<>]+|[a-z]+".r

  println(eval("3 + 4.0"))
  println(eval("3 / 4"))
  println(eval("3 = 3.0"))
  println(eval("3 div 12"))
  println(eval("3 div 10"))

  def eval(exp: String) = {
    val toks = tokens findAllMatchIn exp map (_.matched) toArray

    def num(s: String): Number =
      if (s contains '.')
        s.toDouble
      else
        BigInt(s) match {
          case n if n.isValidInt => n.toInt.asInstanceOf[Number]
          case n                 => n
        }

    val op = toks(1)

    if (Set("+", "-", "*", "/", "%", "^") contains op)
      PrecisionDAL.compute(Symbol(op), num(toks.head), num(toks(2)))
    else
      PrecisionDAL.relate(Symbol(op), num(toks.head), num(toks(2)))
  }

}
