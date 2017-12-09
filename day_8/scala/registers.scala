import java.io.File

import scala.io.Source

object app {

  import model._

  // This just represents a parsed line
  case class CommandL(
    register: String,
    op: String,
    value1: Int,
    testRegister: String,
    condition: String,
    value2: Int
  )

  def main(args: Array[String]): Unit = {
    val fileName =
      "input"
      //"test_input"

    val lines = Source.fromFile(new File(s"..${File.separator}$fileName")).getLines.toList

    // Could make the cond optional..
    val CommandR = """(\w+)\s+(inc|dec)\s+(-?\d+)\s+if\s+(\w+)\s*(==|!=|>=|<=|>|<)\s*(-?\d+)""".r

    val commandLines = lines
      .map { _.trim } // don't want to fool with lead/trail whitespace
      .map {
        case CommandR(reg, op, v1, testReg, cond, v2) =>
          CommandL(reg, op, v1.toInt, testReg, cond, v2.toInt)
      }

    val commands = commandLines.map { l => Command(l)}

    val registers = commands.foldLeft (Set[String]()) {
      case (memo, cmd) => memo ++ Set(cmd.register, cmd.cond.register)
    }.toList
    .sorted

    val initialVals = registers.map { r => (r, 0) }.toMap

    // Don't really have to fold twice, but I think this is clear and
    // we don't have to immediately optimize.
    val (finalValues, maxVal) = commands.foldLeft ((initialVals, 0)) { case ((regVals, highestVal), cmd) =>
      val testVal = regVals(cmd.cond.register)
      //println(testVal + " - " + cmd.cond.eval(testVal))
      if (cmd.cond.eval(testVal)) {
        val initVal = regVals(cmd.register)
        val newVal = if (cmd.op == Dec) initVal - cmd.value else initVal + cmd.value
        val currHighestVal = if (newVal > highestVal) newVal else highestVal
        (regVals + ((cmd.register, newVal)), currHighestVal)
      } else {
        (regVals, highestVal)
      }
    }

    println(s"Answer 1: ${finalValues.map(_._2).max}")
    println(s"Answer 2: $maxVal")

    // Probably the most interesting optimization is to realize that if you
    // haven't seen a register and it's used in a condition, then it must be 0.
    // With that knowledge, you only need to fold once.
  }

  // Don't really need this much of a model, but it wasn't
  // that much typing.
  object model {

    object Command {
      def apply(l: CommandL): Command = {
        Command(
          l.register,
          Op(l.op),
          l.value1,
          Condition(l.testRegister, Symbol(l.condition), l.value2)
        )
      }
    }

    case class Command(register: String, op: Op, value: Int, cond: Condition)

    // Scala will supposedly soon get 'good' enums
    object Op {
      def apply(o: String): Op = {
        o.toLowerCase.trim match {
          case "inc" => Inc
          case "dec" => Dec
        }
      }
    }
    sealed trait Op
    case object Inc extends Op
    case object Dec extends Op

    case class Condition(register: String, symbol: Symbol, value: Int) {
      def eval(actualVal: Int): Boolean = {
        symbol match {
          case Eq  => actualVal == value
          case Neq => actualVal != value
          case Gte => actualVal >= value
          case Lte => actualVal <= value
          case Gt  => actualVal >  value
          case Lt  => actualVal <  value
        }
      }
    }

     object Symbol {
      def apply(s: String): Symbol = {
        s.toLowerCase.trim match {
          case "==" => Eq
          case "!=" => Neq
          case ">=" => Gte
          case "<=" => Lte
          case ">"  => Gt
          case "<"  => Lt
        }
      }
    }
    sealed trait Symbol
    case object Eq extends Symbol
    case object Neq extends Symbol
    case object Gte extends Symbol
    case object Lte extends Symbol
    case object Gt extends Symbol
    case object Lt extends Symbol
  }
}
