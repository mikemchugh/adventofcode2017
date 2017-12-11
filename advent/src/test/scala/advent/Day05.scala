package advent

import org.scalatest._

class JumpSpec extends FlatSpec with Matchers {
  "jump" should "be 3 when input is 1 1 1" in {
    Day05.jump(Vector(1, 1, 1), 0, 1) shouldEqual 3
  }

  "jump" should "increment the index's value on each jump" in {
    Day05.jump(Vector(0), 0, 1) shouldEqual 2
  }

  "jump" should "jump using negative values also" in {
    Day05.jump(Vector(1, -1), 0, 1) shouldEqual 3
  }

  "jump" should "produce the expected advent of code example's result" in {
    Day05.jump(Vector(0, 3, 0, 1, -3), 0, 1) shouldEqual 5
  }

  "just do it" should "get me a star" in {
    Day05.justDoIt() shouldEqual 343467
  }
}