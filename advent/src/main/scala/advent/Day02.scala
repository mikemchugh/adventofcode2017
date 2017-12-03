package advent

object Day02 {
  def convert(input: String): List[Array[Int]] = 
    input.split(scala.util.Properties.lineSeparator)
      .map(_.split("\t")
        .map(_.toInt)
      )
      .toList

  def checksum(input: String) : Int = {
    val numbers = convert(input)

    numbers.map(row => row.reduceLeft((x, y) => x max y) - row.reduceLeft((x, y) => x min y)).sum
  }

  def divideRow(row: Array[Int]) : Int = {
    val division = for {
      a <- row
      b <- row
      if a > b && a % b == 0
    } yield if (a > b && a % b == 0) a / b else 0

    division(0)
  }

  def checksumEvenlyDivisable(input: String) : Int = {
    val numbers = convert(input)

    numbers.map(divideRow(_)).sum
  }
}