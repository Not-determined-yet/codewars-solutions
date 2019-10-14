object SumOfDivided {
  def sumOfDivided(lst: Array[Int]): String = {
    import scala.collection.mutable.ArrayBuffer
    def factors(x: Int): ArrayBuffer[Int] = {
      def travel(x: Int, a: Int): ArrayBuffer[Int] =
        if (a * a > x) ArrayBuffer(x) else
          x % a match {
            case 0 => travel(x / a, a) += a
            case _ => travel(x, a + 1)
          }

      travel(x.abs, 2).reverse
    }

    val primes = lst.flatMap(factors(_).distinct).distinct.sorted
    primes.map(x => Array(x, lst.filter(_ % x == 0).sum)).map(_.mkString("(", " ", ")")).mkString
  }
}