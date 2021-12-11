import scala.io.AnsiColor.*

object Day11 extends Day[Day11.Data, Int, Int] {
  def parse(input: String): Data = input.linesIterator.map(_.map(_ - '0').toArray).toArray

  def part1(data: Data): Int = IndexedSeq.iterate((data, 0), 100 + 1)(s => step(s._1)).map(_._2).sum
  def part2(data: Data): Int = {
    var current = data
    var steps = 0

    while(true) {
      val (next, flashes) = step(current)

      if(flashes == data.length * data(0).length) {
        return steps + 1
      }

      current = next
      steps += 1
    }

    ???
  }

  def step(data: Data): (Data, Int) = {
    val a = increment(data)
    val (b, flashes) = flash(a)
    (reset(b), flashes)
  }

  def increment(data: Data): Data = data.map(_.map(_ + 1))
  def flash(data: Data): (Data, Int) = {
    var flashes = 0
    val next = Array.ofDim[Int](data.length, data(0).length)

    for (y <- data.indices; x <- data(y).indices) {
      if(data(y)(x) > 9) {
        flashes += 1
        next(y).update(x, Int.MinValue)
      } else {
        var level = data(y)(x)

        for(dy <- -1 to 1; dx <- -1 to 1) {
          val nx = x + dx
          val ny = y + dy
          if(0 <= ny && ny < data.length && 0 <= nx && nx < data(ny).length) {
            if(data(ny)(nx) > 9) {
              level += 1
            }
          }
        }

        next(y).update(x, level)
      }
    }

    if flashes > 0 then { val x = flash(next); (x._1, x._2 + flashes) } else (next, flashes)
  }
  def reset(data: Data): Data = data.map(x => x.map(o => if o < 0 then 0 else o))

  type Data = Array[Array[Int]]
}
