import scala.util.parsing.combinator.RegexParsers

object Day2 extends ParseLineDay[Day2.Action, Int, Int] {
  def foward: Parser[Action] = """forward""" ~ number  ^^ { case _ ~ x => Action.Forward(x) }
  def down: Parser[Action]   = """down""" ~ number     ^^ { case _ ~ x => Action.Down(x) }
  def up: Parser[Action]     = """up""" ~ number       ^^ { case _ ~ x => Action.Up(x) }
  def model: Parser[Action] = foward | down | up

  def part1(data: Iterator[Action]): Int = {
    val pos = data.foldLeft(Pos(0, 0))((current, action) => {
      action match {
        case Action.Forward(i) => current.copy(x = current.x + i)
        case Action.Down(i) => current.copy(d = current.d + i)
        case Action.Up(i) => current.copy(d = current.d - i)
      }
    })

    pos.x * pos.d
  }
  def part2(data: Iterator[Action]): Int = {
    val pos = data.foldLeft(PosWithAim(0, 0, 0))((current, action) => {
      action match {
        case Action.Forward(i) => current.copy(x = current.x + i, d = current.d + current.a * i)
        case Action.Down(i) => current.copy(a = current.a + i)
        case Action.Up(i) => current.copy(a = current.a - i)
      }
    })

    pos.x * pos.d
  }

  case class Pos(x: Int, d: Int)
  case class PosWithAim(x: Int, d: Int, a: Int)

  enum Action:
    case Forward(i: Int)
    case Down(i: Int)
    case Up(i: Int)
}