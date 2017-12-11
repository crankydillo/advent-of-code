import org.scalatest._

class hashspec
extends FunSpec
with Matchers {

  describe("Hashing") {
    import hash._

    it("should compute sparse hashes") {
      val string = List(0, 1, 2, 3, 4)
      val inputLens = List(3, 4, 1, 5)
      sparseHash(string, inputLens) should be (List(3, 4, 2, 1, 0))
    }

    describe("dense") {

      List(
        ("",         "a2582a3a0e66e6e86e3812dcb672a272"),
        ("AoC 2017", "33efeb34ea91902bb2f59c9920caa6cd"),
        ("1,2,3",    "3efbe78a8d82f29979031a4aa0b16a9d"),
        ("1,2,4",    "63960835bcdc130f0b66d7ff4f6a5a8e")
      ).foreach { case (input, expectedHash) =>
        val addLens = List(17, 31, 73, 47, 23)

        it(s"denseHash(Range(0, 266), '$input' ++ $addLens) = $expectedHash") {
          val finalInput = input.map { _.asInstanceOf[Int] } ++ addLens
          val hash = denseHash(Range(0, 256), finalInput)

          // TODO this toHex stuff should be in a func I guess..
          val toHex = hash.map { num =>
            String.format("%2s", num.toHexString).replace(" ", "0")
          }

          toHex.mkString("") shouldBe (expectedHash)
        }
      }


    }

  }

  describe("CircularList") {

    import CircularList._

    List(
      (List(0, 1, 2, 3, 4), 0, 3, List(0, 1, 2)),
      (List(2, 1, 0, 3, 4), 3, 4, List(3, 4, 0, 1)),
      (List(4, 3, 0, 1, 2), 3, 1, List(3)),
      (List(4, 3, 0, 1, 2), 1, 5, List(1, 2, 3, 4, 0))
    ).foreach { case (l, start, len, expectedList) =>
      it(s"select($l, $start, $len) = $expectedList)") {
        selectPos(l, start, len) shouldBe (expectedList)
      }
    }
  }
}
