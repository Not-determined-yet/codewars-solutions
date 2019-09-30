object MaximunLengthDifference {
  def mxdiflg(a1: List[String], a2: List[String]): Int = {
    if (a1.length == 0 || a2.length == 0) return -1
    val t1 = a1.map(_.length)
    val t2 = a2.map(_.length)
    math.max(t1.max - t2.min, t2.max - t1.min)
  }
}