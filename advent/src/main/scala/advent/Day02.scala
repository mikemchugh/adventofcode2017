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
}