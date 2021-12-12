import scala.collection.mutable

object Day12 extends LineDay[Day12.Link, Int, Int] {
  def parseLine(line: String): Link = line.split("-").map(Cave.apply) match { case Array(a, b) => Link(a, b) }

  def part1(data: Iterator[Link]): Int = {
    paths(adjacency(data), false).size
  }
  def part2(data: Iterator[Link]): Int = paths(adjacency(data), true).size

  def adjacency(data: Iterator[Link]): Map[Cave, Set[Cave]] = {
    val b = mutable.Map[Cave, Set[Cave]]()

    for(link <- data) {
      b.update(link.start, b.getOrElse(link.start, Set[Cave]()) + link.end)
      b.update(link.end, b.getOrElse(link.end, Set[Cave]()) + link.start)
    }

    b.toMap
  }

  def paths(links: Map[Cave, Set[Cave]], allowDoubleSmall: Boolean): Set[List[Cave]] = paths(links, List(Cave.Start), allowDoubleSmall)

  def paths(links: Map[Cave, Set[Cave]], current: List[Cave], allowDoubleSmall: Boolean): Set[List[Cave]] = {
    var result = Set[List[Cave]]()

    val next = links(current.last).filter(s => s != Cave.Start && s != Cave.End)
    val canEnd = links(current.last).contains(Cave.End)

    if(canEnd) {
      result = result + current.appended(Cave.End)
    }

    for(n <- next) {
      val maxSmallVisits = if allowDoubleSmall then { 2 } else { 1 }
      val nextHasNotBeenVisitedTooMuch = if n.isSmall then { current.count(_ == n) < maxSmallVisits } else { true }

      if (nextHasNotBeenVisitedTooMuch) {
        val continueToAllowDoubleSmall = if n.isSmall then { allowDoubleSmall && !current.contains(n) } else { allowDoubleSmall }

        val nextPath = current.appended(n)
        val nextResults = paths(links, nextPath, continueToAllowDoubleSmall)

        result = result concat nextResults
      }
    }

    result
  }


  case class Link(start: Cave, end: Cave)
  case class Cave(name: String) {
    def isSmall: Boolean = name(0).isLower
  }
  object Cave {
    val Start: Cave = Cave("start")
    val End: Cave = Cave("end")
  }
}
