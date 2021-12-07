object Day7 extends Day[List[Int], Int, Int] {
  def parse(input: String): List[Int] = input.split(',').map(_.toInt).toList

  def part1(data: List[Int]): Int = optimal(data, identity)
  def part2(data: List[Int]): Int = optimal(data, triangle)

  private def optimal(data: List[Int], cost: Int => Int) = (data.min to data.max).map(t => score(data, t, cost)).min
  private def score(positions: List[Int], target: Int, cost: Int => Int) = positions.map(p => cost((p - target).abs)).sum
  private def triangle(dist: Int) = dist * (dist + 1) / 2
}
