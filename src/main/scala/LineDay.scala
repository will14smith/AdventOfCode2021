abstract class LineDay[Model, Result1, Result2] extends Day {
  def main(args: Array[String]): Unit = {
    run("Part 1", part1)
    run("Part 2", part2)
  }

  def run[B](name: String, calc: Iterator[Model] => B): Unit = run(name, lines.map(parse), calc)

  def parse(line: String) : Model

  def part1(data: Iterator[Model]): Result1
  def part2(data: Iterator[Model]): Result2
}