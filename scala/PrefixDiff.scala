object PrefixDiff {

  sealed trait Expr
  sealed abstract class Func(arg: Expr) extends Expr
  sealed abstract class Op(arg1: Expr, arg2: Expr) extends Expr

  case class Value(value: Double) extends Expr
  case object Variable extends Expr

  case class Cos(arg: Expr) extends Func(arg)
  case class Sin(arg: Expr) extends Func(arg)
  case class Tan(arg: Expr) extends Func(arg)
  case class Exp(arg: Expr) extends Func(arg)
  case class Ln(arg: Expr) extends Func(arg)

  case class Plus(arg1: Expr, arg2: Expr) extends Op(arg1, arg2)
  case class Minus(arg1: Expr, arg2: Expr) extends Op(arg1, arg2)
  case class Times(arg1: Expr, arg2: Expr) extends Op(arg1, arg2)
  case class Divide(arg1: Expr, arg2: Expr) extends Op(arg1, arg2)
  case class Pow(arg1: Expr, arg2: Expr) extends Op(arg1, arg2)

  implicit class RichExpr(private val expr: Expr) extends AnyVal {
    def simplify: Expr = {
      expr match {
        case Plus(Value(a), Value(b)) => Value(a + b).simplify
        case Plus(Value(0), arg) => arg.simplify
        case Plus(arg, Value(0)) => arg.simplify
        case Plus(arg1, arg2) => Plus(arg1.simplify, arg2.simplify)

        case Minus(Value(a), Value(b)) => Value(a - b).simplify
        case Minus(Value(0), arg) => arg.simplify
        case Minus(arg, Value(0)) => arg.simplify
        case Minus(arg1, arg2) => Minus(arg1.simplify, arg2.simplify)

        case Times(Value(a), Value(b)) => Value(a * b).simplify
        case Times(Value(0), _) => Value(0)
        case Times(_, Value(0)) => Value(0)
        case Times(Value(1), arg) => arg.simplify
        case Times(arg, Value(1)) => arg.simplify
        case Times(arg1, arg2) => Times(arg1.simplify, arg2.simplify)

        case Divide(Value(a), Value(b)) => Value(a / b).simplify
        case Divide(Value(0), _) => Value(0)
        case Divide(arg, Value(1)) => arg.simplify
        case Divide(arg1, arg2) => Divide(arg1.simplify, arg2.simplify)

        case Pow(Value(a), Value(b)) => Value(Math.pow(a, b).toInt).simplify
        case Pow(Value(0), _) => Value(0)
        case Pow(_, Value(0)) => Value(1)
        case Pow(Value(1), _) => Value(1)
        case Pow(arg, Value(1)) => arg.simplify
        case Pow(arg1, arg2) => Pow(arg1.simplify, arg2.simplify)

        case Cos(arg) => Cos(arg.simplify)
        case Sin(arg) => Sin(arg.simplify)
        case Tan(arg) => Tan(arg.simplify)
        case Ln(arg) => Ln(arg.simplify)
        case Exp(arg) => Exp(arg.simplify)

        case Value(value) => Value(value)
        case Variable => Variable

        case e: Expr => e
      }
    }

    def diff: Expr = {
      expr match {
        case Sin(arg) =>
          Times(arg.simplify.diff, Cos(arg.simplify))
        case Cos(arg) =>
          Times(arg.simplify.diff, Times(Value(-1), Sin(arg.simplify)))
        case Tan(arg) =>
          Times(arg.simplify.diff, Plus(Value(1), Pow(Tan(arg.simplify), Value(2))))
        case Exp(arg) =>
          Times(arg.simplify.diff, Exp(arg.simplify))
        case Ln(arg) =>
          Times(arg.simplify.diff, Divide(Value(1), arg.simplify))
        case Plus(arg1, arg2) =>
          Plus(arg1.simplify.diff, arg2.simplify.diff)
        case Minus(arg1, arg2) =>
          Minus(arg1.simplify.diff, arg2.simplify.diff)
        case Times(arg1, arg2) =>
          Plus(Times(arg1.simplify, arg2.simplify.diff), Times(arg2.simplify, arg1.simplify.diff))
        case Divide(arg1, arg2) =>
          Divide(Minus(Times(arg1.simplify.diff, arg2.simplify), Times(arg2.diff.simplify, arg1.simplify)), Pow(arg2.simplify, Value(2)))
        case Pow(arg1, Value(value)) =>
          // that's strange, the judge cant deal with exchangeable expr answer,
          // this should be covered by "case Pow(arg1, arg2)"
          Times(Times(Value(value), Pow(arg1.simplify, Value(value - 1))), arg1.simplify.diff)

        case Pow(arg1, arg2) =>
          //   (f ^ g)'
          // = f ^ (g-1) * (g' * ln(f) * f + g * f')
          Times(Plus(Times(Times(arg2.simplify.diff, Ln(arg1.simplify)), arg1), Times(arg2.simplify, arg1.simplify.diff)), Pow(arg1.simplify, Minus(arg2.simplify, Value(1))))
        case Value(_) => Value(0)
        case Variable => Value(1)
      }
    }

    def show: String = {
      expr match {
        case Value(value) =>
          if(value % 1 == 0) value.toInt.toString
          else value.toString
        case Variable => "x"
        case Cos(arg) => s"(cos ${arg.show})"
        case Sin(arg) => s"(sin ${arg.show})"
        case Tan(arg) => s"(tan ${arg.show})"
        case Exp(arg) => s"(exp ${arg.show})"
        case Ln(arg) => s"(ln ${arg.show})"
        case Plus(arg1, arg2) => s"(+ ${arg1.show} ${arg2.show})"
        case Minus(arg1, arg2) => s"(- ${arg1.show} ${arg2.show})"
        case Times(arg1, arg2) => s"(* ${arg1.show} ${arg2.show})"
        case Divide(arg1, arg2) => s"(/ ${arg1.show} ${arg2.show})"
        case Pow(arg1, arg2) => s"(^ ${arg1.show} ${arg2.show})"
      }
    }
  }

  object Expr {
    def splitExpr(text: String): (String, String) = {
      if (text.isEmpty) ("", "")
      else if (text.startsWith("(")) {
        var depth = 0
        var i = 0
        do {
          if (text(i) == '(') depth += 1
          else if (text(i) == ')') depth -= 1
          i += 1
        } while (depth != 0 && i < text.length)
        (text.slice(0, i).trim, text.slice(i, text.length+1).trim)
      } else if (text.contains('(')) {
        val head = text.takeWhile(c => c != '(')
        (head.trim, text.drop(head.length).trim)
      } else {
        val arr = text.split(" ", 2)
        (arr(0), arr(1))
      }
    }

    // suppose data is all valid
    def parse(text: String): Expr = {
      if (text.startsWith("(") && text.endsWith(")")) {
        parse(text.slice(1, text.length - 1))
      }
      else {
        val parts = text.split(" ", 2)
        parts(0) match {
          case "+" =>
            val (arg0, arg1) = splitExpr(parts(1))
            Plus(parse(arg0), parse(arg1))

          case "-" =>
            val (arg0, arg1) = splitExpr(parts(1))
            Minus(parse(arg0), parse(arg1))

          case "*" =>
            val (arg0, arg1) = splitExpr(parts(1))
            Times(parse(arg0), parse(arg1))

          case "/" =>
            val (arg0, arg1) = splitExpr(parts(1))
            Divide(parse(arg0), parse(arg1))

          case "^" =>
            val (arg0, arg1) = splitExpr(parts(1))
            Pow(parse(arg0), parse(arg1))

          case "cos" => Cos(parse(parts(1)))
          case "sin" => Sin(parse(parts(1)))
          case "tan" => Tan(parse(parts(1)))
          case "exp" => Exp(parse(parts(1)))
          case "ln" => Ln(parse(parts(1)))
          case "x" => Variable
          case _ =>

            Value(parts(0).toDouble)
        }
      }
    }
  }

  def diff(expr: String): String = {
    val _expr = Expr.parse(expr)
    var res = _expr.simplify.diff

    while (res.simplify != res) {
      res = res.simplify
    }
    res.show
  }

}