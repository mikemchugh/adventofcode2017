package advent

import org.scalatest._
import Day07._

class BaseDiscSpec extends FlatSpec with Matchers {
  "baseDisc" should "be bottomDisc" in {
    val bottomDisc = Disc("disc_bottom", 10, List("disc_middle"))
    val middleDisc = Disc("disc_middle", 7, List("disc_top_left", "disc_top_right"))
    val topLeftDisc = Disc("disc_top_left", 1, List())
    val topRightDisc = Disc("disc_top_right", 1, List())

    val base = baseDisc(topLeftDisc :: bottomDisc :: topRightDisc :: middleDisc :: Nil) 
    base shouldEqual "disc_bottom"
  }

  "partA" should "get me a star" in {
    partA() shouldEqual "vvsvez"
  }
}
