package io.github.edadma.dal

object Testing {

  val tokens = "-?[0-9.]+|[-+*/%^^=<>]+|[a-z]+".r

  def eval(exp: String) = {
    val toks = tokens findAllMatchIn exp map (_.matched) toArray

    def num(s: String): Number =
      if (s contains '.')
        s.toDouble
      else
        BigInt(s) match {
          case n if n isValidInt => n.toInt.asInstanceOf[Number]
          case n                 => n
        }

    val op = toks(1)

    if (Set("+", "-", "*", "/", "%", "^") contains op)
      PrecisionDAL.compute(Symbol(op), num(toks.head), num(toks(2)))
    else
      PrecisionDAL.relate(Symbol(op), num(toks.head), num(toks(2)))
  }

}
