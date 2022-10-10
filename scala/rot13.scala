def rot13(message: String): String = {
  val cMap: Map[Char, Char] = (for {
    char <- ('a' to 'z') ++ ('A' to 'Z')
  } yield {
    val mapChar =
      if (char.isLower) (((char - 'a') + 13) % 26 + 'a').toChar
      else (((char - 'A') + 13) % 26 + 'A').toChar

    char -> mapChar
  }).toMap

  message.map(cMap.withDefault(identity))
}
