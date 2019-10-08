object Psychic {
  def guess(): Double = {
    val field = Class.forName("java.lang.Math$RandomNumberGeneratorHolder").getDeclaredField("randomNumberGenerator")
    field.setAccessible(true)
    val seed = new java.util.Date().getTime
    field.get(null).asInstanceOf[java.util.Random].setSeed(seed)
    val random = new java.util.Random(seed)
    random.nextDouble()
  }
}