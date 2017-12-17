import org.scalatest._

class permutationsSpec extends FunSpec
with Matchers {

  import permutations._

  describe("permutation") {

    describe("dancing") {

      it("should end with participants in a certain order") {
        val initPositions = "abcde".toCharArray.toVector
        val moves = List(Spin(1), SwapPos(3,4), SwapName('e', 'b'))

        // compare by lists to quickly use == for assertion.  ScalaTest can do
        // better with collections..
        dance2(initPositions, moves).toList shouldBe ("baedc".toCharArray.toList)
      }
    }
  }

}
