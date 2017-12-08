package advent

object Day03 {
  trait Direction
  case class Up() extends Direction
  case class Down() extends Direction
  case class Left() extends Direction
  case class Right() extends Direction

  type Coordinate = (Int, Int)

  def nextDirection(currentDirection : Direction) : Direction = currentDirection match {
    case Up() => Left()
    case Left() => Down()
    case Down() => Right()
    case Right() => Up()
  }

  def nextCoordinates(direction : Direction, coordinate : Coordinate) : Coordinate = direction match {
    case Up() => (coordinate._1, coordinate._2 + 1)
    case Left() => (coordinate._1 - 1, coordinate._2)
    case Down() => (coordinate._1, coordinate._2 - 1)
    case Right() => (coordinate._1 + 1, coordinate._2)
  }

  def nextStepCount(direction : Direction, step: Int) : Int = direction match {
    case Up() => step
    case Left() => step + 1
    case Down() => step
    case Right() => step + 1
  }

  def generateSpiral(count : Int, max : Int, step: Int, stepCount: Int, direction: Direction, spiral: List[Coordinate]) : List[Coordinate] = {    
    if (count == max) {
      spiral
    }
    else {
      if (step < stepCount) {
        generateSpiral(
          count + 1, 
          max, 
          step + 1, 
          stepCount, 
          direction, 
          nextCoordinates(direction, spiral.head) :: spiral)
      } else {
        val coordinate = nextCoordinates(direction, spiral.head)
        val newDirection = nextDirection(direction)
        val newStepCount = nextStepCount(newDirection, stepCount)

        generateSpiral(
          count + 1, 
          max, 
          1, 
          newStepCount, 
          newDirection, 
          coordinate :: spiral)
      }
    }
  }

  val last = generateSpiral(1, 361527, 1, 1, Right(), List((0,0))).head
  val distance = math.abs(last._1) + math.abs(last._2)
  println(s"x: ${last._1}, y: ${last._2}")
  println(s"last coordinate is: $distance")
}