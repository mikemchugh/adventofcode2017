package advent

object Day06 {
  def biggestMemoryBlockIndex(input : IndexedSeq[Int]) : Int = {
    input.indices.maxBy(input)
  }

  def reallocationAmount(input : IndexedSeq[Int], indexOfBiggest : Int) : IndexedSeq[Int] = {
    val amount = input(indexOfBiggest)
    val amountToReallocate = amount / input.length
    val updatedList = input.updated(indexOfBiggest, 0).map(_ + amountToReallocate)

    if (amount % input.length == 0) {
      updatedList
    }
    else {
      val additionalAmountToAllocate = amount % input.length
      val indexesToUpdate : IndexedSeq[Int] = for (i <- 1 to additionalAmountToAllocate) yield {
        if (indexOfBiggest + i < input.length) indexOfBiggest + i else indexOfBiggest + i - input.length 
      }
      
      for (i <- 0 until input.length) yield {
        updatedList(i) + indexesToUpdate.count(_ == i)
      }
    }
  }

  def detectInfiniteLoop(map : Map[IndexedSeq[Int], Int], input : IndexedSeq[Int], count : Int)(calc: (Int, Int) => Int) : Int = {
    if (map.getOrElse(input, 0) != 0) { 
      calc(count, map(input))
    }
    else {
      detectInfiniteLoop(map + (input -> count), reallocationAmount(input, biggestMemoryBlockIndex(input)), count + 1)(calc)
    }
  }

  def detectInfiniteLoop(input : IndexedSeq[Int]) : Int = {
    detectInfiniteLoop(Map(input -> 0), input, 0)((x, y) => x)
  }

  def countBetweenDuplicates(input : IndexedSeq[Int]) : Int = {
    detectInfiniteLoop(Map(input -> 0), input, 0)((x, y) => x - y)
  }
}