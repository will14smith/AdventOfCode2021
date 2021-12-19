case class Matrix33(
                     x0: Int, x1: Int, x2: Int,
                     y0: Int, y1: Int, y2: Int,
                     z0: Int, z1: Int, z2: Int) {
  def *(other: Matrix33): Matrix33 = Matrix33(
    x0 * other.x0 + x1 * other.y0 + x2 * other.z0, x0 * other.x1 + x1 * other.y1 + x2 * other.z1, x0 * other.x2 + x1 * other.y2 + x2 * other.z2,
    y0 * other.x0 + y1 * other.y0 + y2 * other.z0, y0 * other.x1 + y1 * other.y1 + y2 * other.z1, y0 * other.x2 + y1 * other.y2 + y2 * other.z2,
    z0 * other.x0 + z1 * other.y0 + z2 * other.z0, z0 * other.x1 + z1 * other.y1 + z2 * other.z1, z0 * other.x2 + z1 * other.y2 + z2 * other.z2,
  )
  def *(other: Position3): Position3 = Position3(other.x * x0 + other.y * x1 + other.z * x2, other.x * y0 + other.y * y1 + other.z * y2, other.x * z0 + other.y * z1 + other.z * z2)
}

object Matrix33 {
  val identity: Matrix33 = Matrix33(
    1, 0, 0,
    0, 1, 0,
    0, 0, 1)

  // loosely based on https://stackoverflow.com/a/16467849
  val roll: Matrix33 = Matrix33(
    1, 0, 0,
    0, 0, 1,
    0, -1, 0
  )
  val turn: Matrix33 = Matrix33(
    0, -1, 0,
    1, 0, 0,
    0, 0, 1
  )

  val axisRotations: List[Matrix33] = {
    val results = List.newBuilder[Matrix33]

    var v = identity

    for (cycle <- 0 until 2) {
      // yield RTTT 3 times
      for (step <- 0 until 3) {
        v = v * roll
        // yield R
        results.addOne(v)

        // yield TTT
        for (i <- 0 until 3) {
          v = v * turn
          results.addOne(v)
        }
      }
      // do RTR
      v = ((v * roll) * turn) * roll
    }

    results.result()
  }
}