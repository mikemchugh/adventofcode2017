package advent

import org.scalatest._

class JumpSpec extends FlatSpec with Matchers {
  "jump" should "be 3 when input is 1 1 1" in {
    Day05.jump(Vector(1, 1, 1)) shouldEqual 3
  }

  "jump" should "increment the offset on each jump" in {
    Day05.jump(Vector(0)) shouldEqual 2
  }

  "jump" should "jump using negative values also" in {
    Day05.jump(Vector(1, -1)) shouldEqual 3
  }

  "jump" should "produce the expected advent of code example's result" in {
    Day05.jump(Vector(0, 3, 0, 1, -3)) shouldEqual 5
  }

  "just do it with normal jump" should "get me a star" in {
    Day05.justDoIt(Day05.jump) shouldEqual 343467
  }

  "jump even strager" should "decrement the offset when offset is 3 or greater" in {
    Day05.jumpEvenStranger(Vector(3, 1, 2, -3)) shouldEqual 4
  }

  "jump even strager" should "produce the expected advent of code example's result" in {
    Day05.jumpEvenStranger(Vector(0, 3, 0, 1, -3)) shouldEqual 10
  }

  "just do it with jump even stranger" should "get me another star" in {
    Day05.justDoIt(Day05.jumpEvenStranger) shouldEqual 24774780
  }
}