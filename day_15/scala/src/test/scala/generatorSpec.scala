import org.scalatest._

class generatorSpec extends FunSpec
with Matchers {

  import generator._

  describe("Simple Gen") {
    val aGen = () => { mkAGen(65) } // has to be function, because we want to reset state
    val aGenExpectedVals = List(1092455, 1181022009, 245556042, 1744312007, 1352636452)
    val bGen = () => { mkBGen(8921) }
    val bGenExpectedVals = List(430625591, 1233683848, 1431495498, 137874439, 285222916)

    describe("Gen") {
      List(
        (aGen(), aGenExpectedVals),
        (bGen(), bGenExpectedVals)
      ).foreach { case (gen, expectedVals) =>
        it(s"This generator should generate ${expectedVals.mkString(", ")}") {
          val generated = Range(0, expectedVals.size).map(i => gen.gen())
          generated.toList shouldBe (expectedVals)
        }
      }
    }

    describe("pairs") {
      val expectedPairs = aGenExpectedVals.zip(bGenExpectedVals)
      val pairs = genPairs(aGen(), bGen()).take(expectedPairs.size)
      pairs shouldBe (expectedPairs)
    }

    it("contest") {
      contest(aGen(), bGen(), 5) shouldBe (1)
    }
  }

  
   
    
  describe("Factoring Gen") {
    val aGen = () => { new FactorPruningGenLong(mkAGen(65), 4) } // has to be function, because we want to reset state
    val aGenExpectedVals = List(1352636452, 1992081072, 530830436, 1980017072, 740335192) 
    val bGen = () => { new FactorPruningGenLong(mkBGen(8921), 8) }
    val bGenExpectedVals = List(1233683848, 862516352, 1159784568, 1616057672, 412269392)

    describe("Gen") {
      List(
        (aGen(), aGenExpectedVals),
        (bGen(), bGenExpectedVals)
      ).foreach { case (gen, expectedVals) =>
        it(s"This generator should generate ${expectedVals.mkString(", ")}") {
          val generated = Range(0, expectedVals.size).map(i => gen.gen())
          generated.toList shouldBe (expectedVals)
        }
      }
    }

    it("contest") {
      contest(aGen(), bGen(), 5 * 1000 * 1000) shouldBe (309)
    }
  }

}
