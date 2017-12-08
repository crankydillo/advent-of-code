import java.io.File

import scala.io.Source

object tower {

  // This just represents a parsed line
  case class DiscL(name: String, weight: Int, supportedDiscs: List[String])

  // The 'real' model
  case class Disc(name: String, weight: Int, supportedDiscs: Set[Disc]) {

    // Not converting the set to a list before mapping totalWeight tripped me up.
    // Might consider folding..
    lazy val totalWeight: Int = weight + supportedDiscs.toList.map(_.totalWeight).sum

    /*
     * Indicates if the supplied disc supported, at any level, by this disc
     */
    def isSupported(d: Disc): Boolean =
      supportedDiscs.contains(d) || supportedDiscs.exists { _.isSupported(d) }
  }

  def main(args: Array[String]): Unit = {
    val fileName =
      "input"
      //"test_input"

    val lines = Source.fromFile(new File(s"..${File.separator}$fileName")).getLines

    // Regex-based parser combinator would make this much clearer..
    val DiscR = """(\w+)\s?\((\d+)\)(?:\s?->\s?(.*?)\s?(\w+))?""".r

    // I do want to fail if parse fails
    val discLines = lines
      .map { _.trim } // don't want to fool with lead/trail whitespace
      .map {
        case DiscR(name, weight, null, null) => DiscL(name, weight.toInt, Nil)
        case DiscR(name, weight, firstSupportedDiscs, lastSupportedDisc) =>
          val supported = firstSupportedDiscs.split(""",\s?""") :+ lastSupportedDisc
          DiscL(name, weight.toInt, supported.toList)
      }

    val discLineMap = discLines.map { dl => dl.name -> dl }.toMap

    // Don't really need all this, but it was fun and feels right for a part 2:)
    def toDisc(dl: DiscL): Disc = {
      Disc(
        dl.name,
        dl.weight,
        dl.supportedDiscs.map { n => toDisc(discLineMap(n)) }.toSet
      )
    }

    val discMap = discLineMap.map { case (n, dl) => n -> toDisc(dl) }

    val supportingDiscs = discMap.filterNot { _._2.supportedDiscs.isEmpty }

    val base = supportingDiscs.find {
      case (_, d) => !supportingDiscs.exists { _._2.supportedDiscs(d) }
    }

    println(base.map(_._1))

    val unbalanced = discMap.filter { case (n, d) =>
      val supportedWeightGroups = d.supportedDiscs.groupBy(_.totalWeight)
      if (supportedWeightGroups.size > 1) {
        supportedWeightGroups.foreach { case (w, ds) =>
          // This should contain the answer, but a human has to parse it.
          // Need to compute the answer.
          println(s"""$n. ${d.weight} - $w - ${ds.map(d => s"${d.name}:${d.weight}")}""")
        }
        println("----")
      }
      supportedWeightGroups.size > 1
    }.map { _._2 }.toList

    unbalanced.foreach { d =>
      println(d.name)
    }

    println(discMap("tghfe"))

    val leaf = unbalanced.filter { d => (unbalanced.filterNot(_ == d)).exists { _.isSupported(d) } }

  }

}
