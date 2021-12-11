import scala.io.AnsiColor.*

object Day11 extends Day[Grid[Int], Int, Int] {
  def parse(input: String): Grid[Int] = Grid(input.linesIterator.map(_.map(_ - '0').toArray).toArray)

  def part1(data: Grid[Int]): Int = steps(data).take(100 + 1).map(_._2).sum
  def part2(data: Grid[Int]): Int = steps(data).indexWhere(_._2 == data.w * data.h)

  def steps(data: Grid[Int]): Iterator[(Grid[Int], Int)] = Iterator.iterate((data, 0))(a => step(a._1))
  def step(data: Grid[Int]): (Grid[Int], Int) = flash(increment(data)).applyLeft(reset)

  def increment(data: Grid[Int]): Grid[Int] = data.map(_ + 1)
  def flash(data: Grid[Int]): (Grid[Int], Int) = {
    var flashes = 0
    val next = data.map((l, p) => {
      if(l > 9) {
        flashes += 1
        Int.MinValue
      } else {
        l + data.neighbours(p).count(data(_) > 9)
      }
    })

    if flashes > 0 then flash(next).applyRight(_ + flashes) else (next, flashes)
  }
  def reset(data: Grid[Int]): Grid[Int] = data.map(_.clampLower(0))

  extension (g: Grid[Int])
    def neighbours(p: Position): Seq[Position] = p.neighbours.filter(g.isValid)
}
