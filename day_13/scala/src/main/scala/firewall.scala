import java.io.File

import scala.collection.mutable.{
  ArrayBuffer, HashMap
}
import scala.io.Source

// As normal, I'm not validating input
object firewall {

  def main(args: Array[String]): Unit = {
    val f = new File("../input")
    val input = Source.fromFile(f).getLines.toList
    val layers = input.filterNot(_.trim.isEmpty).map(parseLine)

    println(severity(firewall(layers)))

    val a2 = freedom(firewall(layers))
    println("a2: " + a2)

    val incLayers = Range(0, a2).foldLeft (firewall(layers)) { (memo, _) => incScanners(memo) }
    println(passThrough(incLayers.toStream))
  }

  /**
   * Move a packet through the firewall.
   * @return the layers in which the packet was caught.
   */
  def passThrough(firewall: Stream[Option[Layer]]): Stream[Layer] = {

    firewall match {
      case s if s.isEmpty     => Stream.empty
      case Some(l) #:: ls =>
        if (l.scannerLoc == 0) l +: passThrough(incScanners(ls))
        else passThrough(incScanners(ls))
      case _ => passThrough(incScanners(firewall.drop(1)))
    }
  }

  def passThrough(firewall: List[Option[Layer]]): Stream[Layer] =
    passThrough(firewall.toStream)

  def severity(firewall: List[Option[Layer]]): Int = {
    passThrough(firewall).map(l => l.idx * l.depth).sum
  }

  /**
   * Given a firewall, determine how long you must delay before the packet can
   * get through without being caught.
   */
  def freedom(firewall: List[Option[Layer]]): Int = {
    // Horribly inefficient in both time and space:(
    // TODO fix that!

    type Prune = (Int, Stream[Int]) => Stream[Int]

    def h(firewall: List[Option[Layer]], loc: Int, pruningFn: Prune): Int = {
      firewall match {
        case Some(l) :: Nil =>
          val currAllowables = Stream.from(1).map(_+loc).filterNot(_ % (l.depth*2-2) == 0)
          pruningFn(loc, currAllowables).head
        case Some(l) :: ls =>
          val newPruningFn: Prune = (lastIdx, s) => {
            pruningFn(loc, s.map(_ - (lastIdx - loc)).filterNot(i => (i) % (l.depth*2-2) == 0))
          }
          h(ls, loc+1, newPruningFn)
        case _ => h(firewall.tail, loc+1, pruningFn)
      }
    }

    h(firewall, 0, (_, s) => s)
  }

  def incScanners(firewall: List[Option[Layer]]): List[Option[Layer]] = incScanners(firewall.toStream).toList
  def incScanners(firewall: Stream[Option[Layer]]): Stream[Option[Layer]] = firewall.map(_.map(_.inc))

  def parseLine(line: String): Layer = {
    val Array(left, right) = line.split("""\s*:\s*""")
    Layer(idx = left.toInt, depth = right.toInt)
  }

  def parse(input: String): Seq[Option[Layer]] =
    firewall(input.trim.split("\n").map { l => parseLine(l) }.toList)

  def firewall(layers: List[Layer]): List[Option[Layer]] = {
    val layerMap = layers.map { l => l.idx -> l }.toMap
    Range(0, layers.last.idx + 1).map { layerMap.get _ }.toList
  }

  case class Layer(
    idx: Int,
    depth: Int,
    scannerLoc: Int = 0,
    scannerDir: Dir = Down
  ) {
    def inc = {
      val nextScanDir = 
        if (scannerLoc >= depth - 1) Up
        else if (scannerLoc == 0)    Down
        else                         scannerDir

      this.copy(
        scannerLoc = if (nextScanDir == Up) scannerLoc - 1 else scannerLoc + 1,
        scannerDir = nextScanDir
      )
    }
  }

  sealed trait Dir
  case object Down extends Dir
  case object Up extends Dir
}
