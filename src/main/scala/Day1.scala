import scala.io.Source

object Day1 extends LineDay[Int, Int, Int] {
  def parseLine(input: String): Int = input.toInt
  
  def part1(depths: Iterator[Int]): Int = calculate(depths)
  def part2(depths: Iterator[Int]): Int = calculate(depths.sliding(3).map(_.sum))

  def calculate(depths: Iterator[Int]): Int = depths.foldLeft((Int.MaxValue, 0)) { (acc, depth) => (depth, acc._2 + (if acc._1 < depth then 1 else 0)) }._2
}
