import java.io.File

import scala.io.Source
import scala.util.{Try, Success, Failure}

import fastparse.all._

object streams {

  def main(args: Array[String]): Unit = {
    // input is on first line
    val f = new File("../input")
    val input = Source.fromFile(f).getLines.mkString("").trim
    println(score(input))
    println(garbageCount(input))
  }

  sealed trait Thing {
    def numGroups: Int
    def garbageCount: Int
  }

  case class Group(things: List[Thing] = Nil) extends Thing {
    override def numGroups = 1 + things.map(_.numGroups).sum
    override def garbageCount = things.map(_.garbageCount).sum
  }

  case class Garbage(chars: List[Char]) extends Thing {
    override def numGroups = 0
    override def garbageCount = chars.size
  }

  object parser {

    val thing: P[Thing] = P(group | garbage)

    val canceled = P("!" ~ AnyChar).map(c => None)

    val garbageChar: P[Option[Char]] =
      P( canceled | AnyChar.!.map(c => Some(c.head)))

    val garbage =
      P( "<" ~ (!">" ~ garbageChar).rep ~ ">").map{ s =>
        Garbage(s.toList.flatten) 
      }

    val group: P[Group] =
      P( "{" ~ thing.rep(sep=",".~/) ~ "}").map(ts => Group(ts.toList))

    def parse(s: String) = thing.parse(s)
  }

  def parse(s: String) = parser.parse(s)

  def score(f: File): Try[Int] = score(Source.fromFile(f).getLines.toList)

  def score(things: List[String]): Try[Int] = {
    // Again, we are eating exceptions..  Just slightly better here.
    val scores = things.map { s => (s, score(s)) }
    val failures = scores.collect { case (t, Failure(_)) => t }
    if (!failures.isEmpty) {
      Failure(new Exception(s"The following things could not be scored: ${failures.mkString(", ")}"))
    } else {
      Success(scores.collect { case (_, Success(score)) => score }.sum)
    }
  }

  def score(thingStr: String): Try[Int] = {
    // TODO look into if Parsed is a monad..
    // If that doesn't pan out, don't eat fastparse messages!
   
    def scoreH(thing: Thing, containerScore: Int): Int = {
      thing match {
        case Garbage(_)      => 0
        case Group(Nil)      => containerScore
        case Group(children) => containerScore + children.map {c => scoreH(c, containerScore + 1) }.sum
      }
    }

    doIfParse(thingStr) { t => scoreH(t, 1) }
  }

  def garbageCount(thingStr: String): Try[Int] = {
    doIfParse(thingStr) { _.garbageCount }
  }

  private def doIfParse[T](thingStr: String)(fn: Thing => T): Try[T] = {
    parse(thingStr) match {
      case Parsed.Success(t, _) => Success(fn(t))
      case _ => Failure(new Exception(s"Failed to parse"))
    }
  }
}
