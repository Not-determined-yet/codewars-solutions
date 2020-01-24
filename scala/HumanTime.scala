object HumanTime {

  def formatDuration(seconds: Int): String = seconds match {
    case 0 => "now"
    case n: Int => covertTime(n).toString
  }

  case class covertTime(n: Int) {
    val (year, day, hour, minute, second) = (
      n / (365 * 24 * 60 * 60),
      n % (365 * 24 * 60 * 60) / (24 * 60 * 60),
      n % (24 * 60 * 60) / (60 * 60),
      n % (60 * 60) / 60,
      n % 60)

    override def toString: String = {
      def wrapper(name: String, n: Int): String = {
        if (n == 0) ""
        else if (n == 1) n + " " + name
        else n + " " + name + "s"
      }

      val namesAndInt =
        List("year", "day", "hour", "minute", "second") zip List(year, day, hour, minute, second)

      val res = namesAndInt.map(x => wrapper(x._1, x._2)).filter(_.length > 0)
      res.length match {
        case 1 => res.mkString
        case n: Int => res.dropRight(1).mkString(", ") + " and " + res.last
      }

    }
  }

}