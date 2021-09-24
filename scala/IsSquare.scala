object IsSquare {
  def isSquare(x: Int): Boolean = {
    val qx =  Math.sqrt(x)
    qx % 1 == 0
  }
}