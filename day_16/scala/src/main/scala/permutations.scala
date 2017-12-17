import java.io.File

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.io.Source

// As normal, I'm not validating input
object permutations {

  def main(args: Array[String]): Unit = {
    val f = new File("../input")
    val input = Source.fromFile(f).getLines.mkString("\n").trim
    val moveStrs = input.split(",")
    val moves = parse(moveStrs)
    val initialPositions = mutable.ArrayBuffer[Char]() ++ Range(97, 97+16).map { _.toChar }
    println(dance2(initialPositions, moves).mkString(""))

    var ctr = 0
    var currStart = initialPositions
    val iterations = 1000 * 1000 * 1000
    val printLine = 1000 * 1000

    def output(): Unit= { println(s"$ctr - ${new java.util.Date}") }

    while (ctr < 1000 * 1000 * 1000) {
      if (ctr % printLine == 0) output()
      currStart = dance2(currStart, moves)
      ctr = ctr + 1
    }
    println(currStart.mkString(""))
  }

  def repeatedDance(): Unit = {
    // If a dance has , you should be able to
    // eliminate all dances between (or something like that)
  }

  def dance5(): Unit = {
    // see where the positions of the end dance are relative to the start.
    // Then make those changes 1 billion times.  My goodness, how could I miss
    // that!  Will come back to this!
  }


  def dance4(): Unit = {
    // Can we add moves together??
  }

  def dance3(): Unit = {
    // Elimininate steps that cancel out e.g. Swap(0,1) -> Swap(1,0) is a noop
  }

  def dance2(posChars: mutable.ArrayBuffer[Char], moves: Seq[Move]): mutable.ArrayBuffer[Char] = {

    // This is a little faster.  Ironically, I used an IDE to debug.

    val size = posChars.size
    
    def spin(i: Int): Unit = {
      val tmps = Range(size-i, size).map { i => posChars(i) }
      (size-1 to i by -1).foreach { k =>
        posChars(k) = posChars(k-i)
      }

      var ctr = 0
      tmps.foreach { char =>
        posChars(ctr) = char
        ctr = ctr + 1
      }
    }

    def swapPos(i1: Int, i2: Int): Unit = {
      val tmp = posChars(i1)
      posChars(i1) = posChars(i2)
      posChars(i2) = tmp
    }

    def swapName(c1: Char, c2: Char): Unit = {
      var i1 = -1
      var i2 = -1
      posChars.zipWithIndex.foreach { case (c, idx) =>
        if (c == c1)      i1 = idx
        else if (c == c2) i2 = idx
      }
      swapPos(i1, i2)
    }

    moves.foreach { move =>
      move match {
        case Spin(i)          => spin(i)
        case SwapPos(i1, i2)  => swapPos(i1, i2)
        case SwapName(n1, n2) => swapName(n1, n2)
      }
      /*
      if (initialPositions.toSet != posChars.toSet) {
        throw new Exception(s"$move - $posChars")
      }
      */
    }
    posChars
  }

  /**
   * Do a dance.
   * @return positions after dance is done.
   */
  def dance(initialPositions: Vector[Char], moves: Seq[Move]): Vector[Char] = {

    // This impl is sort of the 'obvious' solution on top of Scala's collections library
    // Unfortunately, it's not very performant.  I believe if we worked with a Map of
    // char -> position, we could do something much more efficient.  That said,
    // as long as problem 2 completes, I will not be addressing this yet.  I still
    // haven't done part 2 of day 14.

    def spin(i: Int, ps: Vector[Char]) = ps.takeRight(i) ++ ps.take(ps.size-i)

    def swapPos(i1: Int, i2: Int, ps: Vector[Char]) = {
      if (i1 == i2) {
        ps
      } else {
        val (lower, upper) = if (i1 < i2) (i1, i2) else (i2, i1)
        (ps.take(lower) :+ ps(upper)) ++ (ps.slice(lower+1, upper) :+ ps(lower)) ++ ps.takeRight(ps.size-upper-1)
      }
    }

    def swapName(c1: Char, c2: Char, ps: Vector[Char]) =
      swapPos(ps.indexOf(c1), ps.indexOf(c2), ps)

    moves.foldLeft (initialPositions) { (currState, move) =>
      move match {
        case Spin(i)          => spin(i, currState)
        case SwapPos(i1, i2)  => swapPos(i1, i2, currState)
        case SwapName(n1, n2) => swapName(n1, n2, currState)
      }
    }

  }

  def parse(moveStrs: Seq[String]): Seq[Move] = {
    import regex._
    moveStrs.map {
      case SpinR(i)          => Spin(i.toInt)
      case SwapPosR(i1, i2)  => SwapPos(i1.toInt, i2.toInt)
      case SwapNameR(n1, n2) => SwapName(n1, n2)
    }
  }

  object regex {
    val SpinR       = """s(\d+)""".r
    val SwapPosR  = """x(\d+)/(\d+)""".r
    val SwapNameR = """p([a-zA-Z])/([a-zA-Z])""".r
  }

  sealed trait Move
  case class Spin(size: Int) extends Move
  case class SwapPos(idx1: Int, idx2: Int) extends Move
  case class SwapName(c1: Char, c2: Char) extends Move
  object SwapName {
    def apply(s1: String, s2: String): SwapName = {
      // validate single char..
      def firstChar(s: String) = s.toCharArray.head
      SwapName(firstChar(s1), firstChar(s2))
    }
  }

}
