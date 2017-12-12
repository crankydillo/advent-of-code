import org.scalatest._

class mazeSpec
extends FunSpec
with Matchers {

  describe("Mazes") {
    import maze._

    describe("Steps to reach child") {

      List(
        (List(NE,NE,NE), 3),
        (List(NE,NE,SW,SW), 0),
        (List(NE,NE,S,S), 2),
        (List(SE,SW,SE,SW,SW), 3)
      ).foreach { case (directions, expectedSteps) =>
        it(s"""${directions.mkString(",")} = $expectedSteps steps""") {
          steps(directions) shouldBe (expectedSteps)
        }
      }
    }
  }
}
