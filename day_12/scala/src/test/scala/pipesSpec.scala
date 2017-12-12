import org.scalatest._

class pipesSpec
extends FunSpec
with Matchers {

  describe("pipes") {
    import pipes._

    it("given the input, 0 should be connected to 6 pipes") {
      connected(0, input).size shouldBe (6)
    }

    it("the input should have 2 groups") {
      groups(parse(input)).size shouldBe (2)
    }
  }

  val input = """
0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
"""

}
