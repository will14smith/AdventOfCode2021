object Day14 extends Day[(String, Map[String, String]), Long, Long] {
  def parse(input: String): (String, Map[String, String]) = input.split("\r?\n\r?\n") match { case Array(h, m) => (h, parseBody(m)) }
  def parseBody(input: String): Map[String, String] = input.linesIterator.map(parseLine).toMap
  def parseLine(input: String): (String, String) = input.split(" -> ") match { case Array(a, b) => (a, b) }

  def part1(data: (String, Map[String, String])): Long = count(Seq.iterate(toPairs(data._1), 10 + 1)(s => expand(s, data._2)).last)
  def part2(data: (String, Map[String, String])): Long = count(Seq.iterate(toPairs(data._1), 40 + 1)(s => expand(s, data._2)).last)

  def toPairs(input: String): Map[String, Long] = {
    input.sliding(2).concat(List(s"${input.head}", s"${input.last}")).toList.groupMapReduce(identity)(_ => 1L)(_+_)
  }

  def expand(pairs: Map[String, Long], mappings: Map[String, String]): Map[String, Long] = {
    pairs.iterator.flatMap(s => {
      mappings.get(s._1) match {
        case Some(b) => List((s"${s._1.head}$b", s._2), (s"$b${s._1.tail}", s._2))
        case None => List(s)
      }
    }).toList.groupMapReduce(_._1)(_._2)(_+_)
  }

  def count(pairs: Map[String, Long]): Long = {
    val letters = pairs.iterator.flatMap(s => {
      s._1.iterator.map((_, s._2))
    }).toList.groupMapReduce(_._1)(_._2)(_+_)
    
    (letters.values.max / 2) - (letters.values.min / 2)
  }
}
