import scala.collection.mutable

object Day15 extends Day[Grid[Int], Int, Int] {
  def parse(input: String): Grid[Int] = Grid(input.linesIterator.map(_.map(_ - '0').toArray).toArray)

  def part1(data: Grid[Int]): Int = {
    val path = search(data)(Position(0, 0), Position(data.w - 1, data.h - 1), estimate(data))
    // data.debug(path.contains(_))
    path.tail.map(data(_)).sum
  }
  def part2(data: Grid[Int]): Int = {
    val actualData = buildGrid(data)

    val path = search(actualData)(Position(0, 0), Position(actualData.w - 1, actualData.h - 1), estimate(actualData))
    // actualData.debug(path.contains(_))
    path.tail.map(actualData(_)).sum
  }

  def buildGrid(data: Grid[Int]): Grid[Int] = {
    val multiplier = 5
    val actual = Array.ofDim[Int](data.h * multiplier, data.w * multiplier)

    for(p <- data.keys) {
      for(y <- 0 until multiplier) {
        val ay = p.y + y * data.h
        for(x <- 0 until multiplier) {
          val ax = p.x + x * data.w
          val value = data(p) + x + y

          actual(ay).update(ax, if value > 9 then value - 9 else value)
        }
      }
    }

    Grid(actual)
  }

  def estimate(data: Grid[Int])(node: Position, goal: Position): Int = (node.x - goal.x).abs + (node.y - goal.y).abs
  def buildPath(data: Grid[Int])(previous: Map[Position, Position], current: Position): List[Position] = {
    var node = current
    var totalPath = List(node)

    while(previous.contains(node)) {
      node = previous(node)
      totalPath = node :: totalPath
    }

    totalPath
  }
  def search(data: Grid[Int])(start: Position, goal: Position, h: (Position, Position) => Int): List[Position] = {
    val previous = mutable.Map[Position, Position]()

    val gScore = mutable.Map.from(data.keys.map(_ -> Int.MaxValue))
    val fScore = mutable.Map.from(data.keys.map(_ -> Int.MaxValue))
    val open = mutable.Set[Position]()

    gScore.update(start, 0)
    fScore.update(start, h(start, goal))
    open.add(start)

    while(open.nonEmpty) {
      val current = open.minBy(fScore(_))
      open.remove(current)

      if (current == goal) return buildPath(data)(previous.toMap, current)

      for(neighbor <- data.neighbours(current)) {
        // tentativeGScore is the distance from start to the neighbor through current
        val tentativeGScore = gScore(current) + data(neighbor)
        if(tentativeGScore < gScore(neighbor)) {
          // This path to neighbor is better than any previous one. Record it!
          previous.update(neighbor, current)
          gScore.update(neighbor, tentativeGScore)
          fScore.update(neighbor, tentativeGScore + h(neighbor, goal))

          if(!open.contains(neighbor)) {
            open.add(neighbor)
          }
        }
      }
    }
//
//    // Open set is empty but goal was never reached
//    return failure

    ???
  }

  extension (g: Grid[Int])
    def neighbours(p: Position): Seq[Position] = p.orthogonal.filter(g.isValid)
}
