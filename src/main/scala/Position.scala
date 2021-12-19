case class Position(x: Int, y: Int) {
  def neighbours: Seq[Position] = for(dy <- -1 to 1; dx <- -1 to 1) yield Position(x + dx, y + dy)
  def orthogonal: Seq[Position] = Seq(copy(x = x + 1), copy(x = x - 1), copy(y = y + 1), copy(y = y - 1))

  def +(other: Position): Position = Position(x + other.x, y + other.y)
}

case class Position3(x: Int, y: Int, z: Int) {
  def +(other: Position3): Position3 = Position3(x + other.x, y + other.y, z + other.z)
  def -(other: Position3): Position3 = Position3(x - other.x, y - other.y, z - other.z)
}

object Position3 {
  val identity: Position3 = Position3(0, 0, 0)
}