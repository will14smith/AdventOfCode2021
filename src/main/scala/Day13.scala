object Day13 extends Day[Day13.Model, Int, Int] {
  def parse(input: String): Model = input.split("\r?\n\r?\n") match { case Array(p, i) => Model(buildGrid(parsePositions(p)), parseInstructions(i)) }
  def parsePositions(input: String): List[Position] = input.linesIterator.map(_.split(",") match { case Array(x, y) => Position(x.toInt, y.toInt) }).toList
  def parseInstructions(input: String): List[Instruction] = input.linesIterator.map(l => {
    if l.startsWith("fold along y=") then { Instruction.Y(l.substring(13).toInt) } else { Instruction.X(l.substring(13).toInt) }
  }).toList

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
  def part2(data: Model): Int = {
    val result = data.instructions.foldLeft(data.marks)(fold)

    result.debug(result(_), if _ then "#" else ".")

    0
  }

  def fold(marks: Grid[Boolean], instruction: Instruction): Grid[Boolean] = {
    val newGrid = instruction match {
      case Instruction.X(coord) => Grid.empty[Boolean](Math.max(coord, marks.w - coord - 1), marks.h)
      case Instruction.Y(coord) => Grid.empty[Boolean](marks.w, Math.max(coord, marks.h - coord - 1))
    }

    for(p <- marks.keys) {
      val mappedP = instruction match {
        case Instruction.X(coord) => Position(m(p.x, coord, marks.w), p.y)
        case Instruction.Y(coord) => Position(p.x, m(p.y, coord, marks.h))
      }

      newGrid.update(mappedP, newGrid(mappedP) || marks(p))
    }

//    for(p <- newGrid.keys) {
//      newGrid.update(p, marks(p))
//
//      instruction match {
//        case Instruction.X(coord) => newGrid.update(p, marks(p) || marks(Position(marks.w - p.x - 1, p.y)))
//        case Instruction.Y(coord) => newGrid.update(p, marks(p) || marks(Position(p.x, marks.h - p.y - 1)))
//      }
//    }

    newGrid
  }

  def m(p: Int, c: Int, d: Int): Int = {
    val bias = 2 * c - (d - 1)
    val m = if p < c then p else 2 * c - p
    val x = if bias < 0 then m - bias else m
    if x == c then 0 else x
  }

  enum Instruction:
    case X(coord: Int)
    case Y(coord: Int)
  case class Model(marks: Grid[Boolean], instructions: List[Instruction])
}
