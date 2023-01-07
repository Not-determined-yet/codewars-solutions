object MorseDecoder {
  import MorseCodes.morseCodes

  def decode(msg: String): String = {
    msg.trim.split("   ").map(_.split(" ").map(morseCodes).mkString).mkString(" ")
  }

  def decodeBits(bits: String): String = {
    val trimmed = bits.dropWhile(_ == '0').reverse.dropWhile(_ == '0').reverse
    val tokens = trimmed.split("(?<=(.))(?!\\1)")

    val codeLens = tokens.distinct.filter(_.contains("1")).map(_.length)
    val spaceLens = tokens.distinct.filter(_.contains("0")).map(_.length)

    val timeUnit = if (spaceLens.isEmpty) {
      codeLens.min
    } else {
      val l = codeLens.min

      if (spaceLens.forall(x => (x % l == 0) || (x % (3 * l) == 0) || (x % (7 * l) == 0))) l
      else l / 3
    }

    val dotLen = timeUnit
    val minPauseLen = timeUnit
    val medPauseLen = timeUnit * 3

    def decodeBit(token: String): String = {
      if (token.contains("0")) {
        if (token.length == minPauseLen) ""
        else if (token.length == medPauseLen) " "
        else "   "
      } else {
        if (token.length == dotLen) "."
        else "-"
      }
    }

    tokens.map(decodeBit).mkString
  }

  def decodeMorse(morseCode: String): String = {
    morseCode.trim.split("   ").map(_.split(" ").map(morseCodes).mkString).mkString(" ")
  }
}