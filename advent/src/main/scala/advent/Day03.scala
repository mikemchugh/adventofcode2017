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

  def coordinateValue(coordinate : Coordinate, values : Map[(Coordinate), Int]) : Int = {
    values.getOrElse(coordinate, 0)
  }

  def calculateCoordinateValue(coordinate : Coordinate, values : Map[(Coordinate), Int]) = {
    val value = (coordinateValue((coordinate._1 - 1, coordinate._2), values) // left
      + coordinateValue((coordinate._1 + 1, coordinate._2), values) // right
      + coordinateValue((coordinate._1, coordinate._2 + 1), values)  // up
      + coordinateValue((coordinate._1, coordinate._2 - 1), values)  // down
      + coordinateValue((coordinate._1 - 1, coordinate._2 - 1), values)  // bottom left
      + coordinateValue((coordinate._1 + 1, coordinate._2 - 1), values)  // bottom right
      + coordinateValue((coordinate._1 - 1, coordinate._2 + 1), values)  // top left
      + coordinateValue((coordinate._1 + 1, coordinate._2 + 1), values))  // top right

    (coordinate -> value)
  }
  
  val initialMap = ((0, 0) -> 1)
  generateSpiral(1, 250, 1, 1, Right(), List((0,0)))

  def generateSpiralValues(coordinates : List[Coordinate], values : Map[Coordinate, Int]) : Map[Coordinate, Int] = coordinates match {
    case Nil => values
    case x :: xs => generateSpiralValues(xs, values + calculateCoordinateValue(x, values))
  }

  def firstValueHigherThan(number : Int) : Int = {
    val initialMap = Map((0, 0) -> 1)
    val coordinates = generateSpiral(1, 250, 1, 1, Right(), List((0,0)))
    generateSpiralValues(coordinates.reverse.tail, initialMap).filter(c => c._2 > 361527).minBy(_._2)._2
  }
}