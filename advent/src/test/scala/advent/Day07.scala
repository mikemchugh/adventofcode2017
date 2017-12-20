package advent

import org.scalatest._
import Day07._

class DiscsAboveSpec extends FlatSpec with Matchers {
  "discsAbove" should "only contain topDisc" in {
    val bottomDisc = Disc("disc_bottom", 10, List("disc_top"))
    val topDisc = Disc("disc_top", 1, List())

    discsAbove(bottomDisc, topDisc :: bottomDisc :: Nil) shouldEqual topDisc :: Nil
  }
}

class AllAboveSpec extends FlatSpec with Matchers {
  "allAbove" should "all the disc above the base" in {
    val bottomDisc = Disc("disc_bottom", 10, List("disc_middle"))
    val middleDisc = Disc("disc_middle", 7, List("disc_top_left", "disc_top_right"))
    val topLeftDisc = Disc("disc_top_left", 1, List())
    val topRightDisc = Disc("disc_top_right", 1, List())

    val discsAbove = allAbove(topLeftDisc :: bottomDisc :: topRightDisc :: middleDisc :: Nil) 

    discsAbove should contain theSameElementsAs topLeftDisc :: topRightDisc :: middleDisc :: Nil
  }
}

class BaseDiscSpec extends FlatSpec with Matchers {
  "baseDisc" should "be bottomDisc" in {
    val bottomDisc = Disc("disc_bottom", 10, List("disc_middle"))
    val middleDisc = Disc("disc_middle", 7, List("disc_top_left", "disc_top_right"))
    val topLeftDisc = Disc("disc_top_left", 1, List())
    val topRightDisc = Disc("disc_top_right", 1, List())

    val base = baseDisc(topLeftDisc :: bottomDisc :: topRightDisc :: middleDisc :: Nil) 
    base shouldEqual bottomDisc
  }

  "partA" should "get me a star" in {
    partA() shouldEqual Disc("vvsvez", 57, List("utlqx", "xzhdy", "tlmvaep", "nbyij", "fszyth", "zimrf"))
  }
}
