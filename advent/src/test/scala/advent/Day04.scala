package advent

import org.scalatest._

class UniqueSpec extends FlatSpec with Matchers {
  "isUnique" should "be false when list contians duplcate values" in {
    Day04.isUnique(List("aa", "bb", "cc", "dd", "aa")) shouldEqual false
  }

  "isUnique" should "be false when list contians no duplcate values" in {
    Day04.isUnique(List("aa", "bb", "cc", "dd", "ee")) shouldEqual true
  }

  "uniqueCount" should "should be 2 when there are two unique lists" in {
    Day04.uniqueCount(List(List("dd", "ee"), List("aa", "bb"), List("aa", "aa"))) shouldEqual 2
  }
}