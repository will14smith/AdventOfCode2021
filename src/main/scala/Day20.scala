object Day20 extends ParseDay[Day20.Model, Int, Int] {
  def cell: Parser[Boolean] = "." ~> success(false) | "#" ~> success(true)
  def line: Parser[List[Boolean]] = rep(cell)
  def image: Parser[SparseGrid[Boolean]] = repsep(line, "\n") ^^ { SparseGrid.from(_, false) }

  def model: Parser[Model] = line ~ ("\n\n" ~> image) ^^ { case a ~ i => Model(a.toArray, i) }

  def part1(data: Model): Int = count(IndexedSeq.iterate(data, 2 + 1)(enhance).last)
  def part2(data: Model): Int = count(IndexedSeq.iterate(data, 50 + 1)(enhance).last)

  def enhance(input: Model): Model = {
    val newDefault = if input.image.default then input.algorithm.last else input.algorithm.head
    val newImage = SparseGrid[Boolean](newDefault)

    val (lb, ub) = input.image.bounds
    for(y <- lb.y - 1 to ub.y + 1) {
      for(x <- lb.x - 1 to ub.x + 1) {
        val pos = Position(x, y)
        val index = pos.neighbours.map(input.image(_)).foldLeft(0)((a, x) => a << 1 | (if x then 1 else 0))
        newImage.update(pos, input.algorithm(index))
      }
    }

    Model(input.algorithm, newImage)
  }

  def count(input: Model): Int = {
    if(input.image.default) {
      return Int.MaxValue
    }

    var count = 0

    val (lb, ub) = input.image.bounds
    for(y <- lb.y to ub.y) {
      for(x <- lb.x to ub.x) {
        count = count + (if input.image(x, y) then 1 else 0)
      }
    }

    count
  }

  case class Model(algorithm: Array[Boolean], image: SparseGrid[Boolean])
}
