import scala.collection.mutable
import scala.io.AnsiColor.*

object Day9 extends Day[Day9.Data, Int, Int] {
  def parse(input: String): Data = {
    val w = input.indexOf('\n') - 1
    val h = input.count(_ == '\n') + 1

    val map = input.linesIterator.zipWithIndex.flatMap(l => l._1.iterator.zipWithIndex.map(c => (Position(c._2, l._2), Location(c._1 - '0')))).toMap

    Data(w, h, map)
  }

  def part1(data: Data): Int = lowPoints(data).map(data.map).map(_.h + 1).sum
  def part2(data: Data): Int = lowPoints(data).map(basinSize(data)).sorted(Ordering.Int.reverse).take(3).product

  def neighbours(data: Data)(position: Position): Seq[Position] = {
    val positions = Seq.newBuilder[Position]

    if(position.x + 1 < data.w) { positions.addOne(position.copy(x = position.x + 1)) }
    if(position.x - 1 >= 0) { positions.addOne(position.copy(x = position.x - 1)) }
    if(position.y + 1 < data.h) { positions.addOne(position.copy(y = position.y + 1)) }
    if(position.y - 1 >= 0) { positions.addOne(position.copy(y = position.y - 1)) }

    positions.result()
  }

  def lowPoints(data: Data): List[Position] = data.map.keys.filter(isLowPoint(data)).toList
  def isLowPoint(data: Data)(position: Position): Boolean = {
    val loc = data.map(position)
    neighbours(data)(position).forall(loc.h < data.map(_).h)
  }

  def basinSize(data: Data)(position: Position): Int = {
    val basin = mutable.Set(position)
    val search = mutable.Queue(position)

    while(search.nonEmpty) {
      val position = search.dequeue()
      val newPositions = neighbours(data)(position).filter(!basin.contains(_)).filter(data.map(_).h < 9)
      basin.addAll(newPositions)
      search.enqueueAll(newPositions)
    }

    basin.size
  }

  def printData(data: Day9.Data, positionToBoolean: Day9.Position => Boolean): Unit = {
    for(y <- 0 until data.h) {
      for(x <- 0 until data.w) {
        val pos = Position(x, y)

        if(positionToBoolean(pos)) print(RED)
        print(data.map(pos).h)
        print(RESET)
      }
      println()
    }
  }

  case class Position(x: Int, y: Int)
  case class Location(h: Int)
  case class Data(w: Int, h: Int, map: Map[Position, Location])
}
