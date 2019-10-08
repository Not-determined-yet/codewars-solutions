object ValidBraces {

  def validBraces(s: String): Boolean = {
    @scala.annotation.tailrec
    def travel(s: String, stack: String): Boolean = {
      if (s.length == 0) stack.length == 0
      else {
        s.head match {
          case '[' => travel(s.tail, stack + '[')
          case ']' => stack.length > 0 && stack.last == '[' && travel(s.tail, stack.init)
          case '(' => travel(s.tail, stack + '(')
          case ')' => stack.length > 0 && stack.last == '(' && travel(s.tail, stack.init)
          case '{' => travel(s.tail, stack + '{')
          case '}' => stack.length > 0 && stack.last == '{' && travel(s.tail, stack.init)
        }
      }
    }

    travel(s, "")
  }
}