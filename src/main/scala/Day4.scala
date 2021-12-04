import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object Day4 extends InputDay[Day4.Input, Int, Int] {
  val board_size = 5

  def parse(input: String): Day4.Input = {
    val parser = new InputParser()
    parser.parse(parser.input, input).get
  }

  def part1(data: Day4.Input): Int = {
    for(n <- data.input) {
      for(b <- data.boards) {
        b.mark(n)
        if(b.hasBingo) {
          return b.unmarked.sum * n
        }
      }
    }

    ???
  }

  def part2(data: Day4.Input): Int = {
    var finished: List[(Day4.Board, Int)] = List()
    var unfinished = data.boards

    for(n <- data.input) {
      val (newlyFinished, nextUnfinished) = unfinished.partitionMap { b => b.mark(n); if b.hasBingo then Left (b, n) else Right(b) }
      finished = newlyFinished ++ finished
      unfinished = nextUnfinished

      if(unfinished.isEmpty) {
        val (b, n) = finished.head
        return b.unmarked.sum * n
      }
    }

    ???
  }

  class Input(val input: List[Int], val boards: List[Board])
  class Board(var rows: List[List[Int]]) {
    def mark(n: Int): Unit = rows = rows.map(row => row.map(v => if v == n then -1 else v))
    def unmarked: List[Int] = rows.flatten.filter(_ > 0)

    def hasBingo: Boolean = {
      for(x <- 0 until board_size) {
        var xf = true
        var yf = true

        for(y <- 0 until board_size) {
          if(rows(x)(y) >= 0) { xf = false }
          if(rows(y)(x) >= 0) { yf = false }
        }

        if(xf) { return true }
        if(yf) { return true }
      }

      false
    }

  }

  class InputParser extends RegexParsers {
    override protected val whiteSpace: Regex = "[ \r]+".r

    def number: Parser[Int] = """\d+""".r ^^ { _.toInt }

    def commaNumbers: Parser[List[Int]] = rep1sep(number, ",")
    def spaceNumbers: Parser[List[Int]] = rep1(number)

    def inputNumbers: Parser[List[Int]] = commaNumbers
    def bingoBoard: Parser[Board] = "\n" ~> repN(board_size, "\n" ~> spaceNumbers) ^^ { Board(_) }

    def input: Parser[Day4.Input] = inputNumbers ~ rep(bingoBoard) ^^ { case i ~ b => Input(i, b) }
  }
}