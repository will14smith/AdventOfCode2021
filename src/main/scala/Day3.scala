object Day3 extends LineDay[Vector[Int], Int, Int] {
  val numberOfBits = 12

  def parse(line: String): Vector[Int] = line.iterator.map { a => if a == '0' then -1 else 1 }.toVector

  def part1(data: Iterator[Vector[Int]]): Int = {
    val common = most_common(data.toList)
    val gamma = to_int(common)

    gamma * (~gamma & (Math.pow(2, numberOfBits).intValue - 1))
  }
  def part2(data: Iterator[Vector[Int]]): Int = {
    val data_list = data.toList
    var o2 = data_list
    var co2 = data_list

    for (i <- 0 until numberOfBits) {
      if(o2.length > 1) {
        val common = most_common(o2)
        o2 = o2.filter { a => (a(i) == 1) == (common(i) >= 0) }
      }
      if(co2.length > 1) {
        val common = most_common(co2)
        co2 = co2.filter { a => (a(i) == 1) == (common(i) < 0) }
      }
    }

    to_int(o2.head) * to_int(co2.head)
  }

  def most_common(data: List[Vector[Int]]): Vector[Int] = data.reduce { (a, b) => a.lazyZip(b).map(_ + _) }
  def to_int(data: Vector[Int]): Int = data.map { a => if a > 0 then 1 else 0 }.foldLeft(0) { (a, b) => (a << 1) + b }
}
