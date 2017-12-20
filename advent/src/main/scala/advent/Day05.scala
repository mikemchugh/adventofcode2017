package advent

object Day05 {
  def jump(input : IndexedSeq[Int], index : Int, count : Int)(updateOffset : Int => Int) : Int = {
    if (index + input(index) >= input.length) {
      count
    }
    else {
      jump(
        input.updated(index, updateOffset(input(index))),
        index + input(index),
        count + 1)(updateOffset)
    }
  }

  def jump(input : IndexedSeq[Int]) : Int = {
    jump(input, 0, 1) { offset => offset + 1 }
  }

  def jumpEvenStranger(input : IndexedSeq[Int]) : Int = {
    jump(input, 0, 1) { offset => if (offset >= 3) offset - 1 else offset + 1 }
  }

  import scala.io.Source
  def readInput() = {
    for (line <- Source.fromFile("input/day05").getLines) yield line
  }

  def justDoIt(jump: IndexedSeq[Int] => Int) : Int = {
    val input = readInput().map(_.toInt).toIndexedSeq
    jump(input)
  }
}