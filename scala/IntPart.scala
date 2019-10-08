object IntPart {

  def part(n: Long): String = {
    var set: Set[Long] = Set()
    def par(target: Long, max: Long, cur: List[Long]): Unit = {
      if (target == 0)
        set += cur.product
      else if (target > 0) {
        for (i <- 1L to max) {
          par(target - i, i, i :: cur)
        }
      }
    }
    par(n, n, List())

    val res = set.toList.sorted
    val rn = res.length

    val range = res.max - res.min
    val averge = res.sum / rn.toDouble
    val median = (res((rn-1)/2) + res(rn/2)) / 2.0
    f"Range: $range Average: $averge%.2f Median: $median%.2f"
  }
}