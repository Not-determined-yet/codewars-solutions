object BinomialExpansion {
  val N = 100
  val C = Array.ofDim[Int](N, N)

  for (i <- 0 until N) {
    C(i)(0) = 1
    for (j <- 1 to i) {
      C(i)(j) = C(i - 1)(j - 1) + C(i - 1)(j)
    }
  }

  case class Expr(a: Int, x: String, b: Int, n: Int)

  def parse(expr: String): Expr = {
    val arr = expr.split("\\^", 2)

    val (head, tail) = (arr(0), arr(1))
    val text = head.drop(1).dropRight(1)

    val (h, t) = text.span(!_.isLetter)

    val a = if (h.isEmpty) 1
    else if (h.length == 1 && h.head == '-') -1
    else h.toInt


    Expr(a, t.head.toString, t.drop(1).toInt, tail.toInt)
  }

  def expand(expr: String): String = {
    val e = parse(expr)

    val res = for {
      i <- e.n to 0 by -1
      j = e.n - i
    } yield {

      val a = (BigInt(e.a).pow(i) * BigInt(e.b).pow(j) * BigInt(C(e.n)(i)))

      if (a == 0) ""
      else {
        val part1 = {
          if (a == 1) {
            if (i == 0) "+1" else "+"
          }
          else if (a == -1) {
            if (i == 0) "-1" else "-"
          }
          else if (a > 0) s"+$a"
          else s"$a"
        }

        val part2 =
          if (i == 1 || i == 0) "" else s"^$i"

        val variable =
          if (i == 0) "" else e.x

        s"$part1$variable$part2"
      }
    }

    res.mkString.stripPrefix("+")
  }
}
