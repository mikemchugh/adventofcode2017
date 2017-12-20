package advent

object Day07 {
  case class Disc(name : String, weight : Int, discsAboveReferences: Seq[String])

  def baseDisc(discs : Seq[Disc]) : String = {
    val discAbove : Map[String, (Int, Seq[String])] = discs.map{ case Disc(name, weight, discsAboveReferences) => name -> (weight, discsAboveReferences) }.toMap
    val allNames : Set[String] = discAbove.keySet
    val allAboveNames : Set[String] = discAbove.values.flatMap { case (weight, above) => above }.toSet

    (allNames diff allAboveNames).head
  }

  import scala.io.Source
  def readInput() = {
    for (line <- Source.fromFile("input/day07").getLines) yield line
  }

  def makeDisc(lineParts : Seq[String]) : Disc = {
    val name = lineParts(0)
    val weight = lineParts(1).slice(1, lineParts(1).length - 1).toInt
    val references = if (lineParts.length > 2) lineParts.drop(3).map(x => x.replaceAll(",", "")).toList else List()
    Disc(name, weight, references)
  }

  def partA() : String = {
    val discs = readInput().map(line => makeDisc(line.split(" "))).toList
    baseDisc(discs)
  }
}
