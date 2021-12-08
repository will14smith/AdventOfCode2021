object Day8 extends ParseLineDay[Day8.Model, Int, Int] {
  val correctSegments = Map(
    0 -> "abcefg".toSet,
    1 -> "cf".toSet,
    2 -> "acdeg".toSet,
    3 -> "acdfg".toSet,
    4 -> "bcdf".toSet,
    5 -> "abdfg".toSet,
    6 -> "abdefg".toSet,
    7 -> "acf".toSet,
    8 -> "abcdefg".toSet,
    9 -> "abcdfg".toSet,
  )

  def segment: Parser[Segment] = "[a-g]+".r ^^ { _.toSet }
  def model: Parser[Model] = rep1(segment) ~ ("|" ~> rep1(segment)) ^^ { case signals ~ output => Model(signals, output) }

  def part1(data: Iterator[Model]): Int = {
    data.flatMap(_.output).count(x => x.size == 2 || x.size == 3 || x.size == 4 || x.size == 7)
  }
  def part2(data: Iterator[Model]): Int = {
    data.map(solve).sum
  }

  private def solve(data: Model): Int = {
    val mapping = solveMapping(data.signals)

    data.output.map(s => decode(s, mapping)).foldLeft(0){ (a, x) => a * 10 + x }
  }

  private def solveMapping(signals: List[Segment]): Map[Char, Char] = {
    val one = signals.find(_.size == 2).get
    val seven = signals.find(_.size == 3).get
    val four = signals.find(_.size == 4).get
    val seg5 = signals.filter(_.size == 5)
    val seg6 = signals.filter(_.size == 6)

    val a = seven.diff(one).toSeq.head
    val bd = four.diff(one)
    val dg = seg5.map(_.diff(seven)).find(s => s.size == 2).get
    val d = bd.intersect(dg).toSeq.head
    val b = (bd - d).toSeq.head
    val g = (dg - d).toSeq.head
    val c = seg6.map(one.diff(_)).find(s => s.size == 1).get.toSeq.head
    val f = seg6.map(s => s - a - b - c - d - g).find(s => s.size == 1).get.toSeq.head
    val e = seg6.map(s => s - a - b - c - d - g - f).find(s => s.size == 1).get.toSeq.head

    Map(
      a -> 'a',
      b -> 'b',
      c -> 'c',
      d -> 'd',
      e -> 'e',
      f -> 'f',
      g -> 'g'
    )
  }
  private def decode(segment: Segment, mapping: Map[Char, Char]): Int = {
    val decodedSegment = segment.map(mapping(_))
    correctSegments.find(x => x._2 == decodedSegment).get._1
  }

  type Segment = Set[Char]
  case class Model(signals: List[Segment], output: List[Segment])
}
