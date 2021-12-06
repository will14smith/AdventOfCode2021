object Day6 extends InputDay[Map[Int, Long], Long, Long] {
  def parse(input: String): Map[Int, Long] = input.split(',').map(_.toInt).groupMapReduce(identity)(_ => 1L)(_ + _)

  def part1(data: Map[Int, Long]): Long = count(IndexedSeq.iterate(data, 80 + 1)(apply).last)
  def part2(data: Map[Int, Long]): Long = count(IndexedSeq.iterate(data, 256 + 1)(apply).last)

  def apply(data: Map[Int, Long]): Map[Int, Long] = data.toList.flatMap(apply.tupled).groupMapReduce(x => x._1)(_._2)(_ + _)
  def apply(k: Int, v: Long): List[(Int, Long)] = if k == 0 then { List((6, v), (8, v)) } else { List((k - 1, v)) }
  def count(data: Map[Int, Long]): Long = data.values.sum
}
