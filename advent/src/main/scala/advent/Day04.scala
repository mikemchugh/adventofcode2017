package advent

object Day04 {
  import scala.io.Source

  def readInput() = {
    for (line <- Source.fromFile("day04input").getLines) yield line
  }

  def isUnique(line : List[String]) : Boolean = {
    line.size == line.toSet.size
  }

  def uniqueCount(lines : List[List[String]]) : Int = {
    lines.count(isUnique _)
  }

  def sort(input: String) : String = {
    val characters = input.split("")
    scala.util.Sorting.quickSort(characters)
    characters.mkString("")
  }

  def uniqueCount(sort : String => String) : Int = {
    val lines = readInput().map(line => line.split(" ").map(str => sort(str)).toList).toList
    uniqueCount(lines)
  }

  def uniqueCount() : Int = {
    uniqueCount { x => x }
  }

  def uniqueSortedCount() : Int = {
    uniqueCount(sort _)
  }

  println(s"Day04 uniqueCount = ${uniqueCount()}")
  println(s"Day04 uniqueSortedCount = ${uniqueSortedCount()}")
}