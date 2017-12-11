import java.io.File

// As normal, I'm not validating input
object hash {

  val inputLength = List(88,88,211,106,141,1,78,254,2,111,77,255,90,0,54,205)

  def main(args: Array[String]): Unit = {

    {
      val hash = sparseHash(Range(0, 256), inputLength)
      println(hash(0) * hash(1))
    }

    {
      val lens = 
        inputLength.mkString(",").map { _.asInstanceOf[Int] } ++ List(17, 31, 73, 47, 23)
        //"1,2,3".map { _.asInstanceOf[Int] } ++ List(17, 31, 73, 47, 23)
        //"".map { _.asInstanceOf[Int] } ++ List(17, 31, 73, 47, 23)
        //"AoC 2017".map { _.asInstanceOf[Int] } ++ List(17, 31, 73, 47, 23)
        //
      val hash = denseHash(Range(0, 256), lens)

      val toHex = hash.map { num =>
        String.format("%2s", num.toHexString).replace(" ", "0")
      }

      println(toHex.mkString(""))
    }
    
  }

  import CircularList._

  def denseHash(string: Seq[Int], lengths: Seq[Int], rounds: Int = 64): Seq[Int] = {
    val hash = sparseHash(string, lengths, 64)
    hash.grouped(16).map { _.reduce(_ ^ _) }.toList
  }

  def sparseHash(string: Seq[Int], lengths: Seq[Int], rounds: Int = 1): Seq[Int] = {
    val mutableStr = string.toBuffer
    val stringLen = string.size
    var currPos = 0
    var skipSize = 0
    for ( r <- Range(0, rounds) ) {
      var ctr = 0
      while (ctr < lengths.length) {
        val len = lengths(ctr)
        if (len > 1) {
          val positions = selectPos(mutableStr, currPos, len)
          currPos = nextPos(mutableStr, currPos, (len + skipSize) % stringLen, stringLen)
          for { idx1 <- Range(0, positions.size / 2) } {
            val tmp = mutableStr(positions(idx1))
            val idx2 = positions.size - 1 - idx1
            mutableStr(positions(idx1)) = mutableStr(positions(idx2))
            mutableStr(positions(idx2)) = tmp
          }
        } else {
          currPos = nextPos(mutableStr, currPos, (len + skipSize) % stringLen, stringLen)
        }
        skipSize = skipSize + 1
        ctr = ctr + 1
      }
    }
    mutableStr.toList
  }

  def nextPos[T](s: Seq[T], currPos: Int, len: Int, maxLen: Int): Int = {
    val positions = selectPos(s, currPos, len)
    if (positions.isEmpty) {
      currPos
    } else {
      val lastPos = positions.last
      if (lastPos == maxLen - 1) 0
      else lastPos + 1
    }
  }
}

object CircularList {

  /*
   * Returns the positions of a circular list given a position and a length.
   */
  def selectPos[T](ts: Seq[T], start: Int, len: Int): Seq[Int] = {
    val listLen = ts.size
    if (len == 1) {
      List(start)
    } else if ((start + len) < listLen) {
      List.range(start, start+len)
    } else {
      val leftover = len - (listLen - start)
      List.range(start, listLen) ++ List.range(0, leftover)
    }
  }

}

