package advent

object Day05 {
  def jump(input : IndexedSeq[Int], index : Int, count : Int) : Int = {
    if (index + input(index) >= input.length) {
      count
    }
    else {
      jump(
        input.updated(index, input(index) + 1),
        index + input(index),
        count + 1)
    }
  }

  import scala.io.Source
  def readInput() = {
    for (line <- Source.fromFile("day05input").getLines) yield line
  }

  def justDoIt() : Int = {
    val input = readInput().map(_.toInt).toIndexedSeq
    jump(input, 0, 1)
  }
}