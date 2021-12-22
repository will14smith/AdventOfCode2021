import scala.collection.mutable

object Day22 extends ParseLineDay[Day22.Instruction, Long, Long] {
  def action: Parser[Action] = "on" ~> success(Action.On) | "off" ~> success(Action.Off)
  def range: Parser[(Int, Int)] = number ~ (".." ~> number) ^^ { case a ~ b => (a, b) }
  def model: Parser[Instruction] = action ~ ("x" ~> "=" ~> range <~ ",") ~ ("y" ~> "=" ~> range <~ ",") ~ ("z" ~> "=" ~> range) ^^ { case a ~ x ~ y ~ z => Instruction(a, Region(Position3(x._1, y._1, z._1), Position3(x._2, y._2, z._2))) }

  def part1(data: Iterator[Instruction]): Long = {
    val map = data.foldLeft(Map())((m, i) => m(i))

    map.count - map(Instruction(Action.Off, Region(Position3(-50, -50, -50), Position3(50, 50, 50)))).count
  }
  def part2(data: Iterator[Instruction]): Long = data.foldLeft(Map())((m, i) => m(i)).count

  case class Region(topLeft: Position3, bottomRight: Position3) {
    def intersect(other: Region): Option[Region] = {
      val tlx = Math.max(this.topLeft.x, other.topLeft.x)
      val brx = Math.min(this.bottomRight.x, other.bottomRight.x)
      if (tlx > brx) return None

      val tly = Math.max(this.topLeft.y, other.topLeft.y)
      val bry = Math.min(this.bottomRight.y, other.bottomRight.y)
      if (tly > bry) return None

      val tlz = Math.max(this.topLeft.z, other.topLeft.z)
      val brz = Math.min(this.bottomRight.z, other.bottomRight.z)
      if (tlz > brz) return None

      val region = Region(Position3(tlx, tly, tlz), Position3(brx, bry, brz))

      Some(region)
    }

    def count: Long = {
      val diff = bottomRight - topLeft
      (diff.x + 1) * (diff.y + 1) * (diff.z + 1)
    }
  }

  enum Action(val factor: Long) {
    case On extends Action(1)
    case Off extends Action(-1)

    def flip: Action = this match {
      case Action.On => Action.Off
      case Action.Off => Action.On
    }
  }

  case class Instruction(action: Action, region: Region) {
    def intersect(other: Instruction): Option[Instruction] = region.intersect(other.region).map(Instruction(action.flip, _))
  }

  case class Map(regions: Array[Instruction] = Array()) {
    def apply(instruction: Instruction): Map = {
      val newRegions = regions ++ regions.flatMap(_.intersect(instruction))

      instruction.action match {
        case Action.On => Map(newRegions.appended(instruction))
        case Action.Off => Map(newRegions)
      }
    }

    def count: Long = regions.map(r => r.action.factor * r.region.count).sum
  }
}
