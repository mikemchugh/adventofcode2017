package advent

import org.scalatest._

class BiggestMemoryBlockIndexSpec extends FlatSpec with Matchers {
  "BiggestMemoryBlockInde" should "be index 1 when input is 1 2 1" in {
    Day06.biggestMemoryBlockIndex(Vector(1, 2, 1)) shouldEqual 1
  }
}

class ReallocationAmountSpec extends FlatSpec with Matchers {
  "ReallocationAmount" should "be Vector(2, 1, 2, 2) when input is Vector(1, 4, 1, 1)" in {
    Day06.reallocationAmount(Vector(1, 4, 1, 1), 1) shouldEqual Vector(2, 1, 2, 2)
  }

  "ReallocationAmount" should "be Vector(2, 4, 1, 2) when input is Vector(0, 2, 7, 0)" in {
   Day06.reallocationAmount(Vector(0, 2, 7, 0), 2) shouldEqual Vector(2, 4, 1, 2)
 }
} 

class DetectInfiniteLoopSpec extends FlatSpec with Matchers {
  "DetectInfiniteLoop" should "be 5 when input is 0 2 7 0" in {
    Day06.detectInfiniteLoop(Vector(0, 2, 7, 0)) shouldEqual 5
  }

  "DetectInfinitLopp" should "get me a star" in {
    Day06.detectInfiniteLoop(Vector(4, 10, 4, 1, 8, 4, 9, 14, 5, 1, 14, 15, 0, 15, 3, 5)) shouldEqual 12841
  }
}