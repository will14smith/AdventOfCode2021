object Day17 extends ParseDay[Day17.Target, Int, Int] {
  def range: Parser[(Int, Int)] = number ~ (".." ~> number) ^^ { case a ~ b => (a, b) }
  def model: Parser[Target] = ("target area: x=" ~> range) ~ (", y=" ~> range) ^^ { case x ~ y => Target(Position(x.min, y.max), Position(x.max, y.min)) }

  def part1(data: Target): Int = {
    val initialPosition = Position(0, 0)

    var maxH = 0

    for(x <- 0 to data.topLeft.x) {
      for(y <- 0 to data.bottomRight.x*10) {
        val s = validSteps(data)(Probe(initialPosition, Position(x, y)))
        if (didIntersect(data)(s)) {
          maxH = Math.max(maxH, s.map(_.position.y).max)
        }
      }
    }

    maxH
  }
  def part2(data: Target): Int = {
    val initialPosition = Position(0, 0)

    var vectors = Set[Position]()

    for(x <- 0 to data.bottomRight.x) {
      for(y <- data.bottomRight.y to data.bottomRight.x*10) {
        val s = validSteps(data)(Probe(initialPosition, Position(x, y)))
        if (didIntersect(data)(s)) {
          vectors = vectors + Position(x, y)
        }
      }
    }

    vectors.size
  }

  def validSteps(target: Target)(initial: Probe): List[Probe] = steps(initial).takeWhile(p => !target.isPast(p.position)).toList
  def didIntersect(target: Target)(s: List[Probe]): Boolean = s.exists(p => target.isIn(p.position))

  def steps(initial: Probe): Iterator[Probe] = new Iterator[Probe] {
    private var current = initial

    def hasNext: Boolean = true

    def next(): Probe = {
      val newPosition = current.position + current.velocity
      val newVelocity = Position((current.velocity.x - 1).clampLower(0), current.velocity.y - 1)

      current = Probe(newPosition, newVelocity)
      current
    }
  }

  case class Probe(position: Position, velocity: Position)
  case class Target(topLeft: Position, bottomRight: Position) {
    def isIn(p: Position): Boolean = (topLeft.x <= p.x && p.x <= bottomRight.x) && (topLeft.y >= p.y && p.y >= bottomRight.y)
    def isPast(p: Position): Boolean = p.x > bottomRight.x || p.y < bottomRight.y
  }
}
