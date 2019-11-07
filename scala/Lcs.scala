object Lcs {
  def lcs(a: String, b: String): String = {
    val (m, n) = (a.length, b.length)
    val table = Array.fill(m + 1, n + 1)(0)
    for (i <- 1 to m; j <- 1 to n) {
      if (a(i - 1) == b(j - 1))
        table(i)(j) = table(i - 1)(j - 1) + 1
      else
        table(i)(j) = table(i - 1)(j) max table(i)(j - 1)
    }

    var (i, j, res) = (m, n, "")
    while (table(i)(j) > 0) {
      val (top, left, now) = (table(i - 1)(j), table(i)(j - 1), table(i)(j))
      if (top == left) {
        if (top < now) {
          res = a(i - 1) + res
          i -= 1
          j -= 1
        } else i -= 1
      } else {
        if (top == now) i -= 1 else j -= 1
      }
    }
    res
  }
}