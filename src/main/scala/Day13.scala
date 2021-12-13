object Day13 extends ParseDay[Day13.Model, Int, String] {
  private def position = number ~ ("," ~> number) ^^ { case x ~ y => Position(x, y) }
  private def grid = rep1(position <~ "\n") ^^ buildGrid

  private def dir = "x" ~> success(Instruction.X.apply) | "y" ~> success(Instruction.Y.apply)
  private def instruction = "fold along".r ~> (dir <~ "=") ~ number ^^ { case d ~ c => d(c) }
  private def instructions = repsep(instruction, "\n")

  def model: Parser[Model] = grid ~ ("\n" ~> instructions) ^^ { case g ~ i => Model(g, i) }


  private def buildGrid(positions: List[Position]): Grid[Boolean] = {
    val w = positions.map(_.x).max + 1
    val h = positions.map(_.y).max + 1

    val g = Grid.empty[Boolean](w, h)

    for(p <- positions) {
      g.update(p, true)
    }

    g
  }

  def part1(data: Model): Int = {
    val folded = fold(data.marks, data.instructions.head)
    folded.keys.map(folded(_)).count(identity)
  }
  def part2(data: Model): String = {
    val result = data.instructions.foldLeft(data.marks)(fold)

    result.debug(result(_), if _ then "#" else " ")

    "See ^"
  }

  def fold(marks: Grid[Boolean], instruction: Instruction): Grid[Boolean] = {
    val newGrid = instruction match {
      case Instruction.X(coord) => Grid.empty[Boolean](Math.max(coord, marks.w - coord - 1), marks.h)
      case Instruction.Y(coord) => Grid.empty[Boolean](marks.w, Math.max(coord, marks.h - coord - 1))
    }

    for(p <- marks.keys) {
      val mappedP = instruction match {
        case Instruction.X(coord) => Position(map(p.x, coord, marks.w), p.y)
        case Instruction.Y(coord) => Position(p.x, map(p.y, coord, marks.h))
      }

      newGrid.update(mappedP, newGrid(mappedP) || marks(p))
    }

    newGrid
  }

  def map(pos: Int, fold: Int, len: Int): Int = {
    val bias = 2 * fold - (len - 1)
    val m = if pos < fold then pos else 2 * fold - pos
    val x = if bias < 0 then m - bias else m
    if x == fold then 0 else x
  }

  enum Instruction:
    case X(coord: Int)
    case Y(coord: Int)
  case class Model(marks: Grid[Boolean], instructions: List[Instruction])
}
