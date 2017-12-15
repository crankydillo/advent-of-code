import java.io.File

import scala.collection.mutable.{
  ArrayBuffer, HashSet
}
import scala.io.Source

// As normal, I'm not validating input
object knot_hash {

  def main(args: Array[String]): Unit = {
    val f = new File("../input")
    val input = Source.fromFile(f).getLines.mkString("\n").trim
    println(usedSquares(knotHash(input)))
  }

  // Should I move this function to day 10?  Is that what day 10 was producing?
  def knotHash(s: String): List[List[Boolean]] = {
    val rowStrings = Range(0, 128).map(i => s"$s-$i")
    val hashLens = rowStrings.map(_.toList.map(_.asInstanceOf[Int]) ++ List(17, 31, 73, 47, 23))
    val hashedRows = hashLens.map(lens => hash.denseHash(Range(0, 256), lens))
    val binRows = hashedRows.map { row =>
      row.map(num => String.format("%8s", num.toBinaryString).replace(" ", "0"))
    }
    binRows.map(r => r.mkString("").toList.map(d => if (d == '1') true else false)).toList
  }

  def usedSquares(knotHash: List[List[Boolean]]): Int =
    knotHash.map(row => row.map(b => if (b) 1 else 0).sum).sum

  def regions(rows: List[List[Boolean]]): Int = {

    def adjacencies(row: Int, col: Int): Set[(Int, Int)]

    val adjacencySet =
      for {
        (row, rowIdx) <- rows.zipWithIndex
        (col, colIdx) <- row.zipWithIndex
      } yield {
        
      }
  }
}
