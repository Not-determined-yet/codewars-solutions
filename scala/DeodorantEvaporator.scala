object DeodorantEvaporator {
  def evaporator(content: Double, evapPerDay: Int, threshold: Int): Int = {
    def log: Double => Double = math.log _
    val k = (log(threshold/100.0) / log(1-evapPerDay/100.0)).toInt
    if (math.pow(1-evapPerDay/100.0,k) < threshold/100.0) k
    else k + 1
  }
}