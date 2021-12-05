import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object Day5 extends LineDay[Day5.Line, Int, Int] {
  def parse(line: String): Line = {
    val parser = new LineParser()
    parser.parse(parser.line, line).get
  }

  def part1(data: Iterator[Line]): Int = {
    val straight = data.filter { l => l.start.x == l.end.x || l.start.y == l.end.y }.toList
    plotGroupLines(straight).values.count(_ > 1)
  }
  def part2(data: Iterator[Line]): Int = plotGroupLines(data.toList).values.count(_ > 1)

  private def plotGroupLines(lines: List[Line]): Map[Coord, Int] = lines.flatMap(plotLine).groupMapReduce(identity)(_ => 1)(_ + _)
  private def plotLine(line: Line): List[Coord] = {
    val diff = line.end - line.start

    val vector = diff.map(_.sign)
    val steps = diff.map(_.abs).max + 1

    IndexedSeq.iterate(line.start, steps)(_ + vector).toList
  }

  case class Coord(x: Int, y: Int) {
    def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
    def -(other: Coord): Coord = Coord(x - other.x, y - other.y)

    def map(f: Int => Int): Coord = Coord(f(x), f(y))
    def max: Int = Math.max(x, y)

    override def toString: String = s"($x, $y)"
  }
  class Line(val start: Coord, val end: Coord) {
    override def toString: String = s"$start -> $end"
  }

  class LineParser extends RegexParsers {
    def number: Parser[Int] = """\d+""".r ^^ { _.toInt }

    def coord: Parser[Coord] = number ~ ("," ~> number) ^^ { case x ~ y => Coord(x, y) }
    def line: Parser[Line] = coord ~ ("->" ~> coord) ^^ { case s ~ e => Line(s, e) }
  }
}
