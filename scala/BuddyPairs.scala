object BuddyPairs {
  def buddy(start: Long, limit: Long): String = {
    def s(n: Long): Long = {
      var sum = 0L
      for (i <- 1L to (math.sqrt(n)+0.5).toLong if n % i == 0) {
        if (n / i == i) sum += i
        else sum += i + n/i
      }
      sum - n
    }

    for (i <- start to limit) {
      val sn = s(i)
      if (sn > i+1) {
        val sm = s(sn-1)
        if (i == sm - 1) return s"($i ${sn-1})"
      }
    }
    "Nothing"
  }
}