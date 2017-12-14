import org.scalatest._

class firewallSpec extends FunSpec
with Matchers {

  describe("firewall") {
    import firewall._

    it("given the input, the severity should be 24") {
      severity(parse(input).toList) shouldBe (24)
    }

    it("given the input, you should not get caught with a delay of 10") {
      //freedomDelay(parse(input).toList) shouldBe (10)
      val firewall = parse(input).toList
      val wait = freedom(firewall)
      wait shouldBe (10)
      val incLayers = Range(0, wait).foldLeft (firewall) { (memo, _) => incScanners(memo) }
      passThrough(incLayers.toStream).size shouldBe (0)
    }

    List(
      (List("0:2", "1:4"), 1)
      , (List("0:2"), 1)
      , (List("0:243"), 1)
      , (List("0:243", "1:2"), 2)
      , (List("0:2", "1:3", "2:2"), 1)
    ).foreach { case (lines, expected) =>
      it(lines.mkString(", ")) {
        val firewall = parse(lines.mkString("\n")).toList
        val wait = freedom(firewall)
        wait shouldBe (expected)
        val incLayers = Range(0, wait).foldLeft (firewall) { (memo, _) => incScanners(memo) }
        println(passThrough(incLayers.toStream))
        passThrough(incLayers.toStream).size shouldBe (0)
      }
    }
  }

  val input = """
0: 3
1: 2
4: 4
6: 4
"""

}
