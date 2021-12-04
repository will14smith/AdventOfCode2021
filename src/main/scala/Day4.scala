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
        b.remove(n)
        if(b.is_finished) {
          return b.rows.flatten.filter(_ > 0).sum * n
        }
      }
    }

    ???
  }
  def part2(data: Day4.Input): Int = {
    var finished: List[(Day4.Board, Int)] = List()

    for(n <- data.input) {
      for(b <- data.boards) {
        if(!b.is_finished) {
          b.remove(n)
          if (b.is_finished) {
            finished = finished.prepended((b, n))
          }
        }
      }

      if(data.boards.forall(_.is_finished)) {
        val (b, n) = finished.head
        return b.rows.flatten.filter(_ > 0).sum * n
      }
    }

    ???
  }

  class Input(val input: List[Int], val boards: List[Board])
  class Board(var rows: List[List[Int]]) {
    def remove(n: Int): Unit = {
      for(x <- 0 until board_size) {
        for(y <- 0 until board_size) {
          if(rows(x)(y) == n) { rows = rows.updated(x, rows(x).updated(y, -1)) }
        }
      }
    }

    def is_finished: Boolean = {
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

    def comma_numbers: Parser[List[Int]] = repsep(number, ",")
    def space_numbers: Parser[List[Int]] = rep(number)

    def input_numbers = comma_numbers
    def bingo_board = "\n" ~ repN(board_size, "\n" ~ space_numbers ^^ { case _ ~ n => n }) ^^ { case _ ~ b => Board(b) }

    def input: Parser[Day4.Input] = input_numbers ~ rep(bingo_board) ^^ { case i ~ b => Input(i, b) }
  }
}