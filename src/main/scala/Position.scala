case class Position(x: Int, y: Int) {
  def neighbours: Seq[Position] = for(dy <- -1 to 1; dx <- -1 to 1) yield Position(x + dx, y + dy)
  def orthogonal: Seq[Position] = Seq(copy(x = x + 1), copy(x = x - 1), copy(y = y + 1), copy(y = y - 1))

  def +(other: Position): Position = Position(x + other.x, y + other.y)
}

case class Position3(x: Long, y: Long, z: Long) {
  def +(other: Position3): Position3 = Position3(x + other.x, y + other.y, z + other.z)
  def -(other: Position3): Position3 = Position3(x - other.x, y - other.y, z - other.z)

  def manhattan(other: Position3): Long = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs
}

object Position3 {
  val identity: Position3 = Position3(0, 0, 0)
}
