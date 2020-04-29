object RGB {
  def rgb(r: Int, g: Int, b: Int): String = {
    def valid(i: Int) = {
      if (i < 0) 0
      else if (i > 255) 255
      else i
    }

    def convert(i: Int) = {
      val res = valid(i).toHexString
      if (res.length < 2) "0" + res else res
    }

    Array(r, g, b).map(convert).mkString.toUpperCase
  }

  //def rgb(r: Int, g: Int, b: Int): String =
  //  Seq(r, g, b).map(x => f"${255 min (x max 0)}%02X").mkString
}