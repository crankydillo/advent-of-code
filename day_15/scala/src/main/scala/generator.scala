import java.io.File

// As normal, I'm not validating input
object generator {

  // Generator A starts with 116
  // Generator B starts with 299
  private val aGen = () => mkAGen(116)
  private val bGen = () => mkBGen(299)

  def main(args: Array[String]): Unit = {
    println(contest(aGen(), bGen(), iterations = 40 * 1000 * 1000))
    println(
      contest(
        new FactorPruningGenLong(aGen(), 4),
        new FactorPruningGenLong(bGen(), 8),
        iterations = 5 * 1000 * 1000
      )
    )
  }

  def mkAGen(start: Long) = new SimpleGenLong(start, 16807)
  def mkBGen(start: Long) = new SimpleGenLong(start, 48271)

  def genPairs(genA: GenLong, genB: GenLong): Stream[(Long, Long)] =
    Stream.from(0).map(i => (genA.gen(), genB.gen()))


  def contest(genA: GenLong, genB: GenLong, iterations: Int): Long = {
    val mask = 0x00000000ffff
    genPairs(genA, genB).take(iterations)
      .foldLeft (0L) { case (matches, (aVal, bVal)) =>
        if ((aVal & mask) == (bVal & mask)) matches + 1L else matches
      }
  }

 trait GenLong {
   def gen(): Long
 }

  /**
   * Generate a sequence of integers.
   *
   * The generators both work on the same principle. To create its next value,
   * a generator will take the previous value it produced, multiply it by a
   * factor (generator A uses 16807; generator B uses 48271), and then keep the
   * remainder of dividing that resulting product by 2147483647. That final
   * remainder is the value it produces next.
   */
  class SimpleGenLong(start: Long, factor: Long) extends GenLong {
    // TODO do this only with functions..  i.e. No GenLong class
    
    // A lesser challenge is to do this without mutation
    var curr = start

    override def gen(): Long = {
      curr = curr * factor % 2147483647L
      curr
    }
  }

  class FactorPruningGenLong(delegate: GenLong, factor: Long) extends GenLong {
    override def gen(): Long = {
      val delegateVal = delegate.gen()
      if (delegateVal % factor == 0) delegateVal else gen()
    }
  }
}
