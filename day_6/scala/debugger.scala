// Using main so I can use sbt for triggered execution (my 'ci'
// scripts).  nodemon + scala {script} generates hs error files.

object app {

  def main(args: Array[String]): Unit = {
    // Going to do this with mutation.  To show Scala is fine with that:)
    
    var registers = 
      //List(0, 2, 7, 0)
      List(2, 8, 8, 5, 4, 2, 3, 1, 5, 5, 1, 2, 15, 13, 5, 14)

    val previous = 
      //scala.collection.mutable.HashSet(registers)
      scala.collection.mutable.ArrayBuffer(registers)
      
     
    var cycles = 0

    def redistribute(registers: List[Int]): List[Int] = {
      // Not efficient, but input is small
      val maxVal = registers.max
      val biggestIdx = registers.indexOf(maxVal)
      val buffer = registers.toBuffer
      buffer(biggestIdx) = 0
      var ctr = maxVal
      val bufferLen = buffer.size
      var lastIdx = biggestIdx
      while (ctr > 0) {
        val nextIdx = if (lastIdx + 1 == bufferLen) 0 else lastIdx + 1
        if (nextIdx == 0) {
          cycles += 1
        }
        buffer(nextIdx) += 1
        lastIdx = nextIdx
        ctr -= 1
      }
      buffer.toList
    }

    var ctr = 0
    var stop = false
    while(!stop) {
      registers = redistribute(registers)
      if (previous.contains(registers)) {
        stop = true
      }
      previous += registers
      ctr = ctr + 1
    }

    previous.foreach { println }
    println(ctr)
    // only works if previous is insertion-ordered
    println(previous.size - 1 - previous.indexOf(previous.last))
  }
}

