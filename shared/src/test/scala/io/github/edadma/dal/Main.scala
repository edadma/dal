package io.github.edadma.dal

@main def run(): Unit =
  println(eval("3 + 4.0", BasicDAL))
  println(eval("3 / 4", BasicDAL))
  println(eval("3 = 3.0", BasicDAL))
  println(eval("3 div 12", BasicDAL))
  println(eval("3 div 10", BasicDAL))
