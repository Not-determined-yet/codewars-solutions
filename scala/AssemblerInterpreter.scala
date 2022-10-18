import scala.collection.mutable
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object AssemblerInterpreter {

  sealed trait Command

  sealed trait RegOrValue {
    def value(registers: Map[String, Int]): Int
  }

  case class Reg(name: String) extends RegOrValue {
    override def value(registers: Map[String, Int]): Int = registers(name)
  }

  case class Value(value: Int) extends RegOrValue {
    override def value(registers: Map[String, Int]): Int = value
  }

  object RegOrValue {
    def from(token: String): RegOrValue = {
      if (token.forall(_.isLetter)) Reg(token)
      else Value(token.toInt)
    }
  }

  case class Mov(registry: String, target: RegOrValue) extends Command

  case class Inc(registry: String) extends Command

  case class Dec(registry: String) extends Command

  case class Add(registry: String, target: RegOrValue) extends Command

  case class Sub(registry: String, target: RegOrValue) extends Command

  case class Mul(registry: String, target: RegOrValue) extends Command

  case class Div(registry: String, target: RegOrValue) extends Command

  case class Label(label: String) extends Command

  case class Jmp(label: String) extends Command

  case class Cmp(source: RegOrValue, target: RegOrValue) extends Command

  case class Jne(label: String) extends Command

  case class Je(label: String) extends Command

  case class Jg(label: String) extends Command

  case class Jl(label: String) extends Command

  case class Jge(label: String) extends Command

  case class Jle(label: String) extends Command

  case class Call(label: String) extends Command

  case object Ret extends Command

  case object End extends Command

  sealed trait TextOrReg

  case class RegOut(registry: String) extends TextOrReg

  case class Text(text: String) extends TextOrReg

  case class Msg(xs: Seq[TextOrReg]) extends Command

  case class AssemblerState(
                             registries: Map[String, Int],
                             labels: Map[String, Int],
                             commands: Seq[Command],
                             currentCommand: Int,
                             cmp: Int = 0,
                             stack: mutable.Stack[Int] = mutable.Stack.empty[Int],
                             result: String = ""
                           )

  @tailrec
  def execute(state: AssemblerState): Option[String] = {
    if (state.currentCommand < 0 || state.currentCommand >= state.commands.length) None
    else {

      state.commands(state.currentCommand) match {
        case Mov(registry, target) =>
          execute(state.copy(
            registries = state.registries + (registry -> target.value(state.registries)),
            currentCommand = state.currentCommand + 1)
          )

        case Inc(registry) =>
          execute(state.copy(
            registries = state.registries + (registry -> (state.registries(registry) + 1)),
            currentCommand = state.currentCommand + 1)
          )

        case Dec(registry) =>
          execute(state.copy(
            registries = state.registries + (registry -> (state.registries(registry) - 1)),
            currentCommand = state.currentCommand + 1)
          )

        case Add(registry, target) =>
          execute(state.copy(
            registries = state.registries + (registry -> (state.registries(registry) + target.value(state.registries))),
            currentCommand = state.currentCommand + 1)
          )

        case Sub(registry, target) =>
          execute(state.copy(
            registries = state.registries + (registry -> (state.registries(registry) - target.value(state.registries))),
            currentCommand = state.currentCommand + 1)
          )

        case Mul(registry, target) =>
          execute(state.copy(
            registries = state.registries + (registry -> (state.registries(registry) * target.value(state.registries))),
            currentCommand = state.currentCommand + 1)
          )

        case Div(registry, target) =>
          execute(state.copy(
            registries = state.registries + (registry -> (state.registries(registry) / target.value(state.registries))),
            currentCommand = state.currentCommand + 1)
          )

        case Label(_) =>
          execute(state.copy(currentCommand = state.currentCommand + 1))

        case Jmp(label) =>
          execute(state.copy(currentCommand = state.labels(label)))

        case Cmp(source, target) =>
          execute(state.copy(cmp = source.value(state.registries) - target.value(state.registries), currentCommand = state.currentCommand + 1))

        case Jne(label) =>
          if (state.cmp != 0) execute(state.copy(currentCommand = state.labels(label)))
          else execute(state.copy(currentCommand = state.currentCommand + 1))

        case Je(label) =>
          if (state.cmp == 0) execute(state.copy(currentCommand = state.labels(label)))
          else execute(state.copy(currentCommand = state.currentCommand + 1))

        case Jge(label) =>
          if (state.cmp >= 0) execute(state.copy(currentCommand = state.labels(label)))
          else execute(state.copy(currentCommand = state.currentCommand + 1))

        case Jg(label) =>
          if (state.cmp > 0) execute(state.copy(currentCommand = state.labels(label)))
          else execute(state.copy(currentCommand = state.currentCommand + 1))

        case Jle(label) =>
          if (state.cmp <= 0) execute(state.copy(currentCommand = state.labels(label)))
          else execute(state.copy(currentCommand = state.currentCommand + 1))

        case Jl(label) =>
          if (state.cmp < 0) execute(state.copy(currentCommand = state.labels(label)))
          else execute(state.copy(currentCommand = state.currentCommand + 1))

        case Call(label) =>
          execute(state.copy(currentCommand = state.labels(label), stack = state.stack.push(state.currentCommand + 1)))

        case Ret =>
          val stack = state.stack
          val nextCommand = stack.pop()
          execute(state.copy(currentCommand = nextCommand, stack = stack))

        case Msg(xs) =>

          val output = xs.map {
            case RegOut(registry) => state.registries(registry).toString
            case Text(text) => text
          }.mkString

          execute(state.copy(currentCommand = state.currentCommand + 1, result = state.result + output))

        case End => Some(state.result)

      }
    }
  }

  implicit class RichStr(private val str: String) extends AnyVal {
    def dropComment: String = {
      val commentIndex = str.indexOf(';')
      val exclude = str.contains('\'')
      if (commentIndex == -1 || exclude) str
      else str.substring(0, commentIndex)
    }
  }

  def parseMessage(messageText: String): ArrayBuffer[TextOrReg] = {
    var i = 0
    var bracket = 0
    var buffer = ""
    val res = ArrayBuffer.empty[TextOrReg]
    while (i < messageText.length) {
      if (messageText(i) == '\'') {
        if (bracket == 1) {
          if (buffer.nonEmpty) res += Text(buffer)
          buffer = ""
          bracket = 0
          i += 1
        } else {
          if (buffer.trim.nonEmpty) res += RegOut(buffer.trim)
          buffer = ""
          bracket = 1
          i += 1
        }
      } else if (messageText(i) == ';') {
        if (bracket == 0) i = messageText.length
        else {
          i = i + 1
          buffer += messageText(i)
        }
      } else if (messageText(i) == ',') {
        if (bracket == 1) {
          buffer += messageText(i)
        } else {
          if (buffer.trim.nonEmpty) {
            res += RegOut(buffer.trim)
            buffer = ""
          }
        }
        i += 1
      } else {
        buffer += messageText(i)
        i += 1
      }
    }

    if (buffer.trim.nonEmpty) {
      res += RegOut(buffer.trim)
    }

    res
  }


  def assemble(input: String): Array[Command] = {
    val lines = input.split("\n")

    lines.map(_.trim.dropComment.trim).filter(_.nonEmpty)
      .map{ line =>
        val tokens = line.split(" ", 2)
        tokens(0) match {
          case "mov" =>
            val arr = tokens(1).trim.split(", ", 2)
            Mov(arr(0), RegOrValue.from(arr(1)))
          case "inc" => Inc(tokens(1).trim)
          case "dec" => Dec(tokens(1).trim)
          case "add" =>
            val arr = tokens(1).trim.split(", ", 2)
            Add(arr(0), RegOrValue.from(arr(1)))
          case "sub" =>
            val arr = tokens(1).trim.split(", ", 2)
            Sub(arr(0), RegOrValue.from(arr(1)))
          case "mul" =>
            val arr = tokens(1).trim.split(", ", 2)
            Mul(arr(0), RegOrValue.from(arr(1)))
          case "div" =>
            val arr = tokens(1).trim.split(", ", 2)
            Div(arr(0), RegOrValue.from(arr(1)))
          case "cmp" =>
            val arr = tokens(1).trim.split(", ", 2)
            Cmp(RegOrValue.from(arr(0)), RegOrValue.from(arr(1)))
          case "jmp" => Jmp(tokens(1).trim)
          case "jne" => Jne(tokens(1).trim)
          case "je" => Je(tokens(1).trim)
          case "jg" => Jg(tokens(1).trim)
          case "jl" => Jl(tokens(1).trim)
          case "jge" => Jge(tokens(1).trim)
          case "jle" => Jle(tokens(1).trim)
          case "call" => Call(tokens(1).trim)
          case "ret" => Ret
          case "end" => End
          case "msg" =>
            val messageText = tokens(1).trim
            val res = parseMessage(messageText)
            Msg(res.toSeq)
          case label if label.endsWith(":") => Label(label.dropRight(1))
        }
      }
  }



  def interpret(input: String): Option[String] = {
    val commands = assemble(input)
    val labels: Map[String, Int] =
      commands.zipWithIndex
        .collect {
          case (Label(label), index) => label -> index
        }.toMap
    val state = AssemblerState(Map.empty.withDefaultValue(0), labels, commands, 0)

    execute(state)
  }
}

