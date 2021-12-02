import scala.util.parsing.combinator.RegexParsers

object Day2 extends LineDay[Day2.Action, Int, Int] {
  def parse(line: String): Action = {
    val parser = new LineParser()
    parser.parse(parser.action, line).get
  }

  def part1(data: Iterator[Action]): Int = {
    val pos = data.foldLeft(Pos(0, 0))((current, action) => {
      action match {
        case Forward(i) => current.copy(x = current.x + i)
        case Down(i) => current.copy(d = current.d + i)
        case Up(i) => current.copy(d = current.d - i)
      }
    })

    pos.x * pos.d
  }
  def part2(data: Iterator[Action]): Int = {
    val pos = data.foldLeft(PosWithAim(0, 0, 0))((current, action) => {
      action match {
        case Forward(i) => current.copy(x = current.x + i, d = current.d + current.a * i)
        case Down(i) => current.copy(a = current.a + i)
        case Up(i) => current.copy(a = current.a - i)
      }
    })

    pos.x * pos.d
  }

  case class Pos(x: Int, d: Int)
  case class PosWithAim(x: Int, d: Int, a: Int)

  sealed trait Action
  case class Forward(i: Int) extends Action
  case class Down(i: Int) extends Action
  case class Up(i: Int) extends Action
  
  class LineParser extends RegexParsers {
    def number: Parser[Int]    = """\d+""".r             ^^ { _.toInt }
    def foward: Parser[Action] = """forward""" ~ number  ^^ { case _ ~ x => Forward(x) }
    def down: Parser[Action]   = """down""" ~ number     ^^ { case _ ~ x => Down(x) }
    def up: Parser[Action]     = """up""" ~ number       ^^ { case _ ~ x => Up(x) }
    def action: Parser[Action] = foward | down | up
  }
}