object Day20 extends ParseDay[Day20.Model, Int, Int] {
  def cell: Parser[Boolean] = "." ~> success(false) | "#" ~> success(true)
  def line: Parser[Array[Boolean]] = rep(cell) ^^ { _.toArray }
  def image: Parser[Grid[Boolean]] = repsep(line, "\n") ^^ { l => Grid(l.toArray) }

  def model: Parser[Model] = line ~ ("\n\n" ~> image) ^^ { case a ~ i => Model(a, i, false) }

  def part1(data: Model): Int = count(IndexedSeq.iterate(data, 2 + 1)(enhance).last)
  def part2(data: Model): Int = count(IndexedSeq.iterate(data, 50 + 1)(enhance).last)

  def enhance(input: Model): Model = {
    val newDefault = if input.default then input.algorithm.last else input.algorithm.head
    val newImage = Grid.empty[Boolean](input.image.w + 2, input.image.h + 2)

    val image = InfinteGrid(input.image, input.default)

    for(y <- 0 until newImage.h; x <- 0 until newImage.w) {
      val src = Position(x - 1, y - 1)
      newImage.update(x, y, enhance(input.algorithm, image, src))
    }

    Model(input.algorithm, newImage, newDefault)
  }

  def enhance(algorithm: Array[Boolean], image: InfinteGrid[Boolean], p: Position): Boolean = {
    var i = 0

    // unrolling this loop over p.neighbours is much faster
    if(image(p.x - 1, p.y - 1)) i |= 1 << 8
    if(image(p.x + 0, p.y - 1)) i |= 1 << 7
    if(image(p.x + 1, p.y - 1)) i |= 1 << 6
    if(image(p.x - 1, p.y + 0)) i |= 1 << 5
    if(image(p.x + 0, p.y + 0)) i |= 1 << 4
    if(image(p.x + 1, p.y + 0)) i |= 1 << 3
    if(image(p.x - 1, p.y + 1)) i |= 1 << 2
    if(image(p.x + 0, p.y + 1)) i |= 1 << 1
    if(image(p.x + 1, p.y + 1)) i |= 1 << 0

    algorithm(i)
  }

  def count(input: Model): Int = {
    if(input.default) {
      return Int.MaxValue
    }

    var count = 0
    for(y <- 0 until input.image.h; x <- 0 until input.image.w) {
      count = count + (if input.image(x, y) then 1 else 0)
    }
    count
  }

  case class Model(algorithm: Array[Boolean], image: Grid[Boolean], default: Boolean)
}
