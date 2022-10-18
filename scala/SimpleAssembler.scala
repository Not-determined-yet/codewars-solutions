object SimpleAssembler {
  import scala.annotation.tailrec

  sealed trait Command

  sealed trait Target

  object Target {
    def from(token: String): Target = {
      if (token.forall(_.isLetter)) Register(token)
      else Value(token.toInt)
    }
  }

  case class Register(reg: String) extends Target
  case class Value(value: Int) extends Target

  case class Mov(registry: String, value: Target) extends Command

  case class Inc(registry: String) extends Command

  case class Dec(registry: String) extends Command

  case class Jnz(condition: Target, target: Target) extends Command

  def assemble(program: List[String]): List[Command] = {
    program.map { line =>
      val tokens = line.split(" ")
      println(tokens.toList)
      tokens(0) match {
        case "mov" => Mov(tokens(1), Target.from(tokens(2)))
        case "inc" => Inc(tokens(1))
        case "dec" => Dec(tokens(1))
        case "jnz" => Jnz(Target.from(tokens(1)), Target.from(tokens(2)))
      }
    }
  }

  def execute(program: List[Command]): Map[String, Int] = {
    @tailrec
    def loop(program: List[Command], registers: Map[String, Int], pc: Int): Map[String, Int] = {
      if (pc < 0 || pc >= program.length) registers
      else {
        program(pc) match {
          case Mov(registry, target) =>
            val value = target match {
              case Value(v) => v
              case Register(r) => registers(r)
            }
            loop(program, registers + (registry -> value), pc + 1)
          case Inc(registry) => loop(program, registers + (registry -> (registers(registry) + 1)), pc + 1)
          case Dec(registry) => loop(program, registers + (registry -> (registers(registry) - 1)), pc + 1)
          case Jnz(condition, target) =>
            val value = target match {
              case Value(v) => v
              case Register(r) => registers(r)
            }

            val conditionValue = condition match {
              case Value(v) => v
              case Register(r) => registers(r)
            }


            if (conditionValue != 0) loop(program, registers, pc + value)
            else loop(program, registers, pc + 1)
        }
      }
    }
    loop(program, Map().withDefaultValue(0), 0)
  }

  def interpret(program: List[String]): Map[String, Int] = {
    execute(assemble(program))
  }
}