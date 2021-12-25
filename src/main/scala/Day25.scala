object Day25 extends ParseDay[Grid[Day25.Cucumber], Int, Int] {
  def cell: Parser[Cucumber] = '.' ~> success(Cucumber.None) | '>' ~> success(Cucumber.East) | 'v' ~> success(Cucumber.South)
  def line: Parser[Array[Cucumber]] = rep(cell) ^^ { _.toArray }
  def model: Parser[Grid[Cucumber]] = repsep(line, "\n") ^^ { l => Grid(l.toArray) }

  def part1(data: Grid[Cucumber]): Int = {
    Iterator.iterate((data, true))(i => step(i._1)).zipWithIndex.find(!_._1._2).get._2
  }
  def part2(data: Grid[Cucumber]): Int = 0

  def step(input: Grid[Cucumber]): (Grid[Cucumber], Boolean) = {
    val (a, m) = step(input, Cucumber.East)
    val (b, n) = step(a, Cucumber.South)

    (b, m || n)
  }
  def step(input: Grid[Cucumber], direction: Cucumber): (Grid[Cucumber], Boolean) = {
    var output = Grid.empty[Cucumber](input)

    val vector = direction match {
      case Cucumber.None => Position(0, 0)
      case Cucumber.East => Position(1, 0)
      case Cucumber.South => Position(0, 1)
    }

    var anyMoved = false
    for(p <- input.keys) {
      var thisMoved = false
      val a = input(p)
      if (a == direction) {
        val p2 = Position((p.x + vector.x) % input.w, (p.y + vector.y) % input.h)
        val b = input(p2)
        if(b == Cucumber.None) {
          output.update(p, Cucumber.None)
          output.update(p2, a)
          thisMoved = true
          anyMoved = true
        }
      }

      if(!thisMoved && output(p) == null) {
        output.update(p, a)
      }
    }

    (output, anyMoved)
  }

  enum Cucumber:
    case None
    case East
    case South
}
