import scala.annotation.targetName
import scala.io.Source
import scala.util.Using
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

abstract class Day[Model, Result1, Result2] {
  private val pattern = """.*Day(\d+).*""".r
  private def fileName = "inputs/day" + pattern.findFirstMatchIn(getClass.getName).get.group(1)
  private def input : String = Source.fromFile(fileName).mkString.replace("\r\n", "\n")

  def main(args: Array[String]): Unit = {
    run("Part 1", part1)
    run("Part 2", part2)
  }
  
  def parse(input: String) : Model

  def part1(data: Model): Result1
  def part2(data: Model): Result2
  
  private def run[B](name: String, calc: Model => B): Unit = run(name, parse(input), calc)
  private def run[R](name: String, data: Model, calc: Model => R): Unit = {
    val start = System.nanoTime()
    val result = calc(data)
    val end = System.nanoTime()

    println(s"$name: $result in ${Math.round((end - start) / 100000) / 10} ms")
  }
}

abstract class LineDay[Model, Result1, Result2] extends Day[Iterator[Model], Result1, Result2] {
  def parse(line: String) : Iterator[Model] = line.linesIterator.map(parseLine)
  def parseLine(line: String) : Model
}

trait AOCParsers extends RegexParsers {
  override protected val whiteSpace: Regex = "[ \r]+".r

  def number: Parser[Int] = """-?\d+""".r ^^ { _.toInt }

  def commaNumbers: Parser[List[Int]] = rep1sep(number, ",")
  def spaceNumbers: Parser[List[Int]] = rep1(number)
}

abstract class ParseDay[Model, Result1, Result2] extends Day[Model, Result1, Result2] with AOCParsers {
  def parse(line: String) : Model = parse(model, line).get
  def model: Parser[Model]
}

abstract class ParseLineDay[Model, Result1, Result2] extends LineDay[Model, Result1, Result2] with AOCParsers {
  def parseLine(line: String) : Model = parse(model, line).get
  def model: Parser[Model]
}