package advent

object Day07 {
  case class Disc(name : String, weight : Int, discsAboveReferences: Seq[String])

  def discsAbove(disc : Disc, allDiscs : Seq[Disc]) = {
    for {
      aDisc <- allDiscs
      if disc.discsAboveReferences.contains(aDisc.name)
    } yield aDisc
  }

  def allAbove(discs : Seq[Disc]) : Seq[Disc] = {
    discs flatMap (x => discsAbove(x, discs))
  }

  def baseDisc(discs : Seq[Disc]) : Disc = {
    discs filter(allAbove(discs).contains(_) == false) head
  }

  import scala.io.Source
  def readInput() = {
    for (line <- Source.fromFile("day07input").getLines) yield line
  }

  def makeDisc(lineParts : Seq[String]) : Disc = {
    val name = lineParts(0)
    val weight = lineParts(1).slice(1, lineParts(1).length - 1).toInt
    val references = if (lineParts.length > 2) lineParts.drop(3).map(x => x.replaceAll(",", "")).toList else List()
    Disc(name, weight, references)
  }

  def partA() : Disc = {
    val discs = readInput().map(line => makeDisc(line.split(" "))).toList
    baseDisc(discs)
  }
}
