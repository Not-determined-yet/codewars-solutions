object BitCounting {
  def countBits(n: Int): Int = n.toBinaryString.count(_ == '1')
}