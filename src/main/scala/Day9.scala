import scala.collection.mutable
import scala.io.AnsiColor.*

object Day9 extends Day[Grid[Int], Int, Int] {
  def parse(input: String): Grid[Int] = Grid(input.linesIterator.map(_.map(_ - '0').toArray).toArray)

  def part1(data: Grid[Int]): Int = lowPoints(data).map(data.apply).map(_ + 1).sum
  def part2(data: Grid[Int]): Int = lowPoints(data).map(basinSize(data)).sorted(Ordering.Int.reverse).take(3).product

  def lowPoints(data: Grid[Int]): List[Position] = data.keys.filter(isLowPoint(data)).toList
  def isLowPoint(data: Grid[Int])(position: Position): Boolean = data.neighbours(position).forall(data(position) < data(_))

  def basinSize(data: Grid[Int])(position: Position): Int = {
    val basin = mutable.Set(position)
    val search = mutable.Queue(position)

    while(search.nonEmpty) {
      val position = search.dequeue()
      val newPositions = data.neighbours(position).filter(!basin.contains(_)).filter(data(_) < 9)
      basin.addAll(newPositions)
      search.enqueueAll(newPositions)
    }

    basin.size
  }

  extension (g: Grid[Int])
    def neighbours(p: Position): Seq[Position] = p.orthogonal.filter(g.isValid)
}
