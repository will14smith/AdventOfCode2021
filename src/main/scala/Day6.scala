object Day6 extends Day[Map[Int, Long], Long, Long] {
  def parse(input: String): Map[Int, Long] = input.split(',').map(_.toInt).freq

  def part1(data: Map[Int, Long]): Long = count(IndexedSeq.iterate(data, 80 + 1)(apply).last)
  def part2(data: Map[Int, Long]): Long = count(IndexedSeq.iterate(data, 256 + 1)(apply).last)

  def apply(data: Map[Int, Long]): Map[Int, Long] = data.freqFlatMap(apply)
  def apply(k: Int): List[Int] = if k == 0 then { List(6, 8) } else { List(k - 1) }
  def count(data: Map[Int, Long]): Long = data.values.sum
}
