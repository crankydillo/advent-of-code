import java.io.File

import scala.io.Source

// As normal, I'm not validating input
object pipes {

  def main(args: Array[String]): Unit = {
    val f = new File("../input")
    val input = Source.fromFile(f).getLines.toList
    val pipes = input.filterNot(_.trim.isEmpty).map(parseLine).toMap
    println(connected(0, pipes).size)
    println(groups(pipes).size)
  }

  def connected(prog: Int, input: String): Set[Int] = {
    connected(prog, parse(input))
  }

  def connected(prog: Int, input: List[String]): Set[Int] = {
    connected(
      prog,
      input.filterNot(_.trim.isEmpty).map(parseLine).toMap
    )
  }

  def connected(prog: Int, pipes: Map[Int, Set[Int]]): Set[Int] = {
    pipes.get(prog) match {
      case Some(ps) => (ps + prog) ++ ps.flatMap { p => connected(p, pipes - prog) }
      case _        => Set.empty
    }
  }

  def groups(pipes: Map[Int, Set[Int]]): Set[Set[Int]] = {
    pipes.foldLeft (Set.empty[Set[Int]]) { case (memo, (prog, _)) =>
      memo + connected(prog, pipes)
    }
  }

  def parseLine(line: String): (Int, Set[Int]) = {
    val Array(left, right) = line.split(" <-> ")
    val leftProg = left.toInt
    val rightProgs = right.split(", ").map(_.toInt).toSet
    (leftProg -> rightProgs)
  }

  // I've used regexes and parser combinators so far, so for
  // this I'll just use some simple splitting.
  def parse(input: String): Map[Int, Set[Int]] = {
    val trimmedLines = input.trim().split("\n").map(_.trim)
    trimmedLines.foldLeft (Map.empty[Int, Set[Int]]) { (memo, line) =>
      memo + parseLine(line)
    }
  }

}
