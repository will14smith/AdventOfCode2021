import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day22 extends ParseLineDay[Day22.Instruction, Long, Long] {
  def action: Parser[Region => Instruction] = "on" ~> success(Instruction.On.apply) | "off" ~> success(Instruction.Off.apply)
  def range: Parser[(Int, Int)] = number ~ (".." ~> number) ^^ { case a ~ b => (a, b) }
  def model: Parser[Instruction] = action ~ ("x" ~> "=" ~> range <~ ",") ~ ("y" ~> "=" ~> range <~ ",") ~ ("z" ~> "=" ~> range) ^^ { case a ~ x ~ y ~ z => a(Region(Position3(x._1, y._1, z._1), Position3(x._2 + 1, y._2 + 1, z._2 + 1))) }

  def part1(data: Iterator[Instruction]): Long = {
    val map = data.foldLeft(Map())((m, i) => m(i))

    map.count - map(Instruction.Off(Region(Position3(-50, -50, -50), Position3(50 + 1, 50 + 1, 50 + 1)))).count
  }
  def part2(data: Iterator[Instruction]): Long = data.foldLeft(Map())((m, i) => m(i)).count

  case class Region(topLeft: Position3, bottomRight: Position3) {
    def isEmpty: Boolean = topLeft.x >= bottomRight.x || topLeft.y >= bottomRight.y || topLeft.z >= bottomRight.z

    def intersect(other: Region): Option[Region] = {
      val tlx = Math.max(this.topLeft.x, other.topLeft.x)
      val brx = Math.min(this.bottomRight.x, other.bottomRight.x)
      if (tlx >= brx) return None

      val tly = Math.max(this.topLeft.y, other.topLeft.y)
      val bry = Math.min(this.bottomRight.y, other.bottomRight.y)
      if (tly >= bry) return None

      val tlz = Math.max(this.topLeft.z, other.topLeft.z)
      val brz = Math.min(this.bottomRight.z, other.bottomRight.z)
      if (tlz >= brz) return None

      val region = Region(Position3(tlx, tly, tlz), Position3(brx, bry, brz))

      Some(region)
    }

    def remove(other: Region): List[Region] = {
      val top = Region(topLeft, Position3(bottomRight.x, Math.min(other.topLeft.y, bottomRight.y), bottomRight.z))
      val bottom = Region(Position3(topLeft.x, Math.max(topLeft.y, other.bottomRight.y), topLeft.z), bottomRight)

      val left = Region(Position3(topLeft.x, top.bottomRight.y, topLeft.z), Position3(Math.min(bottomRight.x, other.topLeft.x), bottom.topLeft.y, bottomRight.z))
      val right = Region(Position3(Math.max(topLeft.x, other.bottomRight.x), top.bottomRight.y, topLeft.z), Position3(bottomRight.x, bottom.topLeft.y, bottomRight.z))

      val front = Region(Position3(left.bottomRight.x, top.bottomRight.y, topLeft.z), Position3(right.topLeft.x, bottom.topLeft.y, Math.min(other.topLeft.z, bottomRight.z)))
      val back = Region(Position3(left.bottomRight.x, top.bottomRight.y, Math.max(topLeft.z, other.bottomRight.z)), Position3(right.topLeft.x, bottom.topLeft.y, bottomRight.z))

      val regions = List(top, bottom, left, right, front, back).filterNot(_.isEmpty)

      regions
    }

    def removeThisFrom(regions: List[Region]): List[Region] = {
      val result = new mutable.ListBuffer[Region]

      for(r <- regions) {
        val i = intersect(r)
        if (i.isDefined) {
          result.addAll(r.remove(i.get))
        } else {
          result.addOne(r)
        }
      }

      result.toList
    }
  }

  enum Instruction:
    case On(region: Region)
    case Off(region: Region)

  case class Map(regions: List[Region] = List()) {
    def apply(instruction: Instruction): Map = instruction match {
      case Instruction.On(region) => on(region)
      case Instruction.Off(region) => off(region)
    }

    def count: Long = regions.map(r => r.bottomRight - r.topLeft).map(r => r.x * r.y * r.z).sum

    private def on(region: Region): Map = Map(regions ++ regions.foldLeft(List(region))((n, other) => other.removeThisFrom(n)))
    private def off(region: Region): Map = Map(region.removeThisFrom(regions))
  }
}
