import org.scalatest._

class StreamsSpec
extends FunSpec
with Matchers {

  import fastparse.all._
  import streams._

  describe("Parsing") {
    import org.json4s._
    import org.json4s.native.JsonMethods.{ parse => jParse, pretty, render }

    implicit val formats = DefaultFormats

    describe("should parse the following as garbage") {
      List(
        "<>",
        "<random characters>",
        "<<<<>",
        "<{!>}>",
        "<!!>",
        "<!!!>>",
        """<{o"i!a,<{i<a>"""
      ).foreach { case garbageStr =>
        it(garbageStr) {
          val Parsed.Success(g, _) = parse(garbageStr)
          println(g)
          g shouldBe an[Garbage]
        }
      }
    }

    describe("should parse the following as groups") {
      List(
        ("{}", 1),
        ("{{{}}}", 3),
        ("{{},{}}", 3),
        ("{{{},{},{{}}}}", 6),
        ("{<{},{},{{}}>}", 1),
        ("{<a>,<a>,<a>,<a>}", 1),
        ("{{<a>},{<a>},{<a>},{<a>}}", 5),
        ("{{<!>},{<!>},{<!>},{<a>}}", 2)
      ).foreach { case (thingStr, expectedNumGroups) =>
        it(thingStr) {
          val Parsed.Success(t, _) = parse(thingStr)
          t shouldBe an[Group]
          t.numGroups shouldBe expectedNumGroups
        }
      }
    }
  }

  describe("Scoring") {

    List(
      ("{}", 1)
      , ("{{{}}}", 6) // 1 + 2 + 3 = 6.
      , ("{{},{}}", 5) // 1 + 2 + 2 = 5.
      , ("{{{},{},{{}}}}", 16) // 1 + 2 + 3 + 3 + 3 + 4 = 16.
      , ("{<a>,<a>,<a>,<a>}", 1) // 1.
      , ("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9) // 1 + 2 + 2 + 2 + 2 = 9.
      , ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9) // 1 + 2 + 2 + 2 + 2 = 9.
      , ("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3) // 1 + 2 = 3.
    ).foreach { case (thingStr, expectedScore) =>
      it(s"'$thingStr' should have score: $expectedScore") {
        score(thingStr) shouldBe scala.util.Success(expectedScore)
      }
    }
  }

  describe("Garbage counting") {
    List(
      ("<>", 0)
      , ("<random characters>", 17)
      , ("<<<<>", 3)
      , ("<{!>}>", 2)
      , ("<!!>", 0)
      , ("<!!!>>", 0)
      , ("""<{o"i!a,<{i<a>""", 10)
    ).foreach { case (thingStr, expectedGarbage) =>
      it(s"'$thingStr' should have garbage count: $expectedGarbage") {
        garbageCount(thingStr) shouldBe scala.util.Success(expectedGarbage)
      }
    }
  }
}
