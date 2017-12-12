import java.io.File

import scala.io.Source

// As normal, I'm not validating input
object maze {

  def main(args: Array[String]): Unit = {
    val f = new File("../input")
    val input = Source.fromFile(f).getLines.mkString("").trim
    val dirs = input.split(",").map(Direction.apply _)
    println(steps(dirs))

    // Yes, I'm going over it twice...
    val start = Point(0, 0)
    val res = dirs.foldLeft ((Point(0, 0), 0)) { case ((currSpot, longest), dir) =>
      val next = currSpot.move(dir)
      val dist = start.distance(next)
      (next, if (dist > longest) dist else longest)
    }
    println(res)
  }

  def steps(ds: Seq[Direction]): Int = {
    val stop = ds.foldLeft (Point(0, 0)) { (p, d) => p.move(d) }
    Point(0,0).distance(stop)
  }

  case class Point(x: Int, y: Int) {

    def move(d: Direction): Point = {
      d match {
        case N  => Point(x  , y+2)
        case S  => Point(x  , y-2)
        case E  => Point(x+2, y  )
        case W  => Point(x-2, y  )
        case NE => Point(x+1, y+1)
        case SE => Point(x+1, y-1)
        case NW => Point(x-1, y+1)
        case SW => Point(x-1, y-1)
      }
    }

    def distance(p2: Point) = {
      def distH(p1: Point, p2: Point, steps: Int): Int = {
        if (p1 == p2)           steps
        else if (p2.x == p1.x)  steps + (p2.y/2 - p1.y/2).abs
        else if (p2.y == p1.y)  steps + (p2.x/2 - p1.x/2).abs
        else {
          val xDiff = p2.x - p1.x
          val yDiff = p2.y - p1.y
          val min = math.min(xDiff.abs, yDiff.abs)
          val next = 
            Point(
              if (xDiff < 0) -1 * min else min,
              if (yDiff < 0) -1 * min else min
            )
          distH(next, p2, steps + min)
        }
      }
      distH(this, p2, 0) 
    }
  }

  object Direction {
    def apply(s: String): Direction = {
      s.toLowerCase match {
        case "n"  => N 
        case "s"  => S
        case "e"  => E
        case "w"  => W
        case "ne" => NE
        case "nw" => NW
        case "se" => SE
        case "sw" => SW
      }
    }
  }
  sealed trait Direction
  case object N extends Direction
  case object S extends Direction
  case object E extends Direction
  case object W extends Direction
  case object NE extends Direction
  case object SE extends Direction
  case object NW extends Direction
  case object SW extends Direction
}
