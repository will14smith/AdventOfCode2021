object Day17 extends ParseDay[Day17.Target, Int, Int] {
  def range: Parser[(Int, Int)] = number ~ (".." ~> number) ^^ { case a ~ b => (a, b) }
  def model: Parser[Target] = ("target area: x=" ~> range) ~ (", y=" ~> range) ^^ { case x ~ y => Target(Position(x.min, y.max), Position(x.max, y.min)) }

  def part1(data: Target): Int = {
    val initialPosition = Position(0, 0)

    var maxH = 0

    for(v <- initialVelocities(data)(initialPosition)) {
      val s = validSteps(data)(Probe(initialPosition, v))
      if (didIntersect(data)(s)) {
        maxH = Math.max(maxH, s.map(_.position.y).max)
      }
    }

    maxH
  }
  def part2(data: Target): Int = {
    val initialPosition = Position(0, 0)

    var vectors = Set[Position]()

    for(v <- initialVelocities(data)(initialPosition)) {
      val s = validSteps(data)(Probe(initialPosition, v))
      if (didIntersect(data)(s)) {
        vectors = vectors + v
      }
    }

    vectors.size
  }

  def initialVelocities(target: Target)(initial: Position): Seq[Position] = for(x <- initialXVelocities(target)(initial); y <- target.bottomRight.y to -target.bottomRight.y) yield Position(x, y)
  def initialXVelocities(target: Target)(initial: Position): List[Int] = {
    val minDX = target.topLeft.x - initial.x
    val maxDX = target.bottomRight.x - initial.x

    Iterator.iterate(0)(_ + 1).take(maxDX + 1).map(n => triangleSteps(n).takeWhile(_ <= maxDX).exists(_.between(minDX, maxDX))).zipWithIndex.filter(_._1).map(_._2).toList
  }
  def triangleSteps(n: Int): Iterator[Int] = Iterator.iterate(n)(_ - 1).takeWhile(_ > 0).map(s => (s to n).sum)

  def validSteps(target: Target)(initial: Probe): List[Probe] = steps(initial).takeWhile(p => !target.isPast(p.position)).toList
  def didIntersect(target: Target)(s: List[Probe]): Boolean = s.exists(p => target.isIn(p.position))

  def steps(initial: Probe): Iterator[Probe] = Iterator.iterate(initial)(step)
  def step(p: Probe): Probe = Probe(p.position + p.velocity, Position((p.velocity.x - 1).clampLower(0), p.velocity.y - 1))

  case class Probe(position: Position, velocity: Position)
  case class Target(topLeft: Position, bottomRight: Position) {
    def isIn(p: Position): Boolean = (topLeft.x <= p.x && p.x <= bottomRight.x) && (topLeft.y >= p.y && p.y >= bottomRight.y)
    def isPast(p: Position): Boolean = p.x > bottomRight.x || p.y < bottomRight.y
  }
}
