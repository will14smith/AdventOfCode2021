import scala.annotation.tailrec
import scala.collection.mutable

object Day15 extends Day[Grid[Int], Int, Int] {
  def parse(input: String): Grid[Int] = Grid(input.linesIterator.map(_.map(_ - '0').toArray).toArray)

  def part1(data: Grid[Int]): Int = solve(data)
  def part2(data: Grid[Int]): Int = solve(buildGrid(data, 5))

  def solve(data: Grid[Int]): Int = {
    val path = search(data)(Position(0, 0), Position(data.w - 1, data.h - 1), estimate(data))
    path.tail.map(data(_)).sum
  }
  def buildGrid(data: Grid[Int], multiplier: Int): Grid[Int] = Grid.empty[Int](data.h * multiplier, data.w * multiplier).map((_, p) => ((data(Position(p.x % data.w, p.y % data.h)) + p.x / data.w + p.y / data.h - 1) % 9) + 1)

  def estimate(data: Grid[Int])(node: Position, goal: Position): Int = (node.x - goal.x).abs + (node.y - goal.y).abs
  @tailrec
  def buildPath(data: Grid[Int])(previous: Map[Position, Position], current: Position, path: List[Position] = List()): List[Position] = {
    previous.get(current) match {
      case Some(node) => buildPath(data)(previous, node, current :: path)
      case None => current :: path
    }
  }
  def search(data: Grid[Int])(start: Position, goal: Position, h: (Position, Position) => Int): List[Position] = {
    val previous = mutable.Map[Position, Position]()

    val gScore = mutable.Map.from(data.keys.map(_ -> Int.MaxValue))
    val open = mutable.PriorityQueue[(Position, Int)](start -> h(start, goal))(Ordering.by[(Position, Int), Int](_._2).reverse)

    gScore.update(start, 0)

    while(open.nonEmpty) {
      val (current, _) = open.dequeue()

      if (current == goal) return buildPath(data)(previous.toMap, current)

      for(neighbor <- data.neighbours(current)) {
        // tentativeGScore is the distance from start to the neighbor through current
        val tentativeGScore = gScore(current) + data(neighbor)
        if(tentativeGScore < gScore(neighbor)) {
          // This path to neighbor is better than any previous one. Record it!
          previous.update(neighbor, current)
          gScore.update(neighbor, tentativeGScore)
          open.enqueue((neighbor, tentativeGScore + h(neighbor, goal)))
        }
      }
    }

    throw Exception("could not find path")
  }

  extension (g: Grid[Int])
    def neighbours(p: Position): Seq[Position] = p.orthogonal.filter(g.isValid)
}
