import scala.collection.mutable
import scala.io.AnsiColor.*

object Day9 extends Day[Day9.Data, Int, Int] {
  def parse(input: String): Data = {
    Data(input.linesIterator.map(_.map(_ - '0').toArray).toArray)
  }

  def part1(data: Data): Int = lowPoints(data).map(data.apply).map(_ + 1).sum
  def part2(data: Data): Int = lowPoints(data).map(basinSize(data)).sorted(Ordering.Int.reverse).take(3).product

  def neighbours(data: Data)(position: Position): Seq[Position] = {
    Seq(
      position.copy(x = position.x + 1),
      position.copy(x = position.x - 1),
      position.copy(y = position.y + 1),
      position.copy(y = position.y - 1),
    ).filter(p => 0 <= p.x && p.x < data.w && 0 <= p.y && p.y < data.h)
  }

  def lowPoints(data: Data): List[Position] = data.keys.filter(isLowPoint(data)).toList
  def isLowPoint(data: Data)(position: Position): Boolean = {
    val loc = data(position)
    neighbours(data)(position).forall(loc < data(_))
  }

  def basinSize(data: Data)(position: Position): Int = {
    val basin = mutable.Set(position)
    val search = mutable.Queue(position)

    while(search.nonEmpty) {
      val position = search.dequeue()
      val newPositions = neighbours(data)(position).filter(!basin.contains(_)).filter(data(_) < 9)
      basin.addAll(newPositions)
      search.enqueueAll(newPositions)
    }

    basin.size
  }

  def printData(data: Day9.Data, positionToBoolean: Day9.Position => Boolean): Unit = {
    var line = 0

    for(pos <- data.keys) {
      if(line != pos.y) println()
      line = pos.y

      if(positionToBoolean(pos)) print(RED)
      print(data(pos))
      print(RESET)
    }
  }

  case class Position(x: Int, y: Int)
  case class Data(heightmap: Array[Array[Int]]) {
    def w: Int = heightmap(0).length
    def h: Int = heightmap.length

    def apply(p: Position): Int = heightmap(p.y)(p.x)
    def keys: Seq[Position] = for { y <- 0 until h; x <- 0 until w } yield Position(x, y)
  }
}
