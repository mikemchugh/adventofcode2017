package advent

import org.scalatest._

class DirectionSpec extends FlatSpec with Matchers {
  "nextDirection" should "be Left when currentDirection is Up" in {
    Day03.nextDirection(Day03.Up()) shouldEqual Day03.Left()
  }

  "nextDirection" should "be Down when currentDirection is Left" in {
    Day03.nextDirection(Day03.Left()) shouldEqual Day03.Down()
  }

  "nextDirection" should "be Right when currentDirection is Down" in {
    Day03.nextDirection(Day03.Down()) shouldEqual Day03.Right()
  }

  "nextDirection" should "be Up when currentDirection is Right" in {
    Day03.nextDirection(Day03.Right()) shouldEqual Day03.Up()
  }
}

class GenerateSpiralSpec extends FlatSpec with Matchers {
  "generateSpiral" should "can generate the expexted spiral" in {
    Day03.generateSpiral(1, 10, 1, 1, Day03.Right(), List((0,0))) should contain theSameElementsAs 
    List((2,-1), (1,-1), (0,-1), (-1,-1), (-1,0), (-1,1), (0,1), (1,1), (1,0), (0,0)) 
  }
}

class firstValueHigherThanSpec extends FlatSpec with Matchers {
  "firstValueHigherThan" should "be 363010 when number is 361527" in {
    Day03.firstValueHigherThan(361527) shouldEqual 363010
  }
}