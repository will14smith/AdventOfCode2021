import scala.io.Source

object Day1 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("inputs/day1").getLines()
    val depths = parse(lines).toList

    println(s"Part1 ${part1(depths.iterator)}")
    println(s"Part2 ${part2(depths.iterator)}")
  }

  def part1(depths: Iterator[Int]): Int = calculate(depths)
  def part2(depths: Iterator[Int]): Int = calculate(depths.sliding(3).map(_.sum))

  def parse(lines: Iterator[String]): Iterator[Int] = {
    lines.map(_.toInt)
  }

  def calculate(depths: Iterator[Int]): Int = depths.foldLeft((Int.MaxValue, 0)) { (acc, depth) => (depth, acc._2 + (if acc._1 < depth then 1 else 0)) }._2
}
