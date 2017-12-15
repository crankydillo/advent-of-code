import org.scalatest._

class knot_hashSpec extends FunSpec
with Matchers {

  describe("knot_hash") {
    import knot_hash._

    List(
      ("flqrgnkx", 8108)
    ).foreach { case (input, expectedUsed) =>
      it(s"$input should have $expectedUsed used squares") {
        usedSquares(knotHash(input)) shouldBe (expectedUsed)
      }
    }

    List(
      ("flqrgnkx", 1242)
    ).foreach { case (input, expectedUsed) =>
      it(s"$input should have $expectedUsed regions") {
        regions(knotHash(input)) shouldBe (expectedUsed)
      }
    }
  }

}
