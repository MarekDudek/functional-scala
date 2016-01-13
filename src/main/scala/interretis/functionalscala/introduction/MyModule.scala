package interretis.functionalscala.introduction

object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, Factorial.factorial))
  }
}
