object PrimeDecomp {
  import scala.collection.mutable.ArrayBuffer
  def factors(m: Int): String = {
    def factorize(x: Int): ArrayBuffer[Int] = {
      def travel(x: Int, a: Int): ArrayBuffer[Int] =
        if (a * a > x) ArrayBuffer(x) else
          x % a match {
            case 0 => travel(x / a, a) += a
            case _ => travel(x, a + 1)
          }
      travel(x, 2)
    }

    factorize(m).groupBy(identity).toList.sortBy(_._1)
      .map(x => if (x._2.length > 1) s"(${x._1}**${x._2.length})" else s"(${x._1})").mkString

  }
}