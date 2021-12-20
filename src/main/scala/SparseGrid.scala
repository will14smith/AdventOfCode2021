import scala.collection.mutable

class SparseGrid[A](val default: A) {
  private val cells = new mutable.HashMap[Position, A]

  private var topLeftBound = Position(0, 0)
  private var bottomRightBound = Position(0, 0)

  def apply(x: Int, y: Int): A = apply(Position(x, y))
  def apply(p: Position): A = cells.getOrElse(p, default)

  def update(x: Int, y: Int, value: A): Unit = update(Position(x, y), value)
  def update(p: Position, value: A): Unit = {
    if (value == default) {
      cells.remove(p)
    } else {
      if(topLeftBound.x > p.x) { topLeftBound = Position(p.x, topLeftBound.y) }
      if(topLeftBound.y > p.y) { topLeftBound = Position(topLeftBound.x, p.y) }
      if(bottomRightBound.x < p.x) { bottomRightBound = Position(p.x, bottomRightBound.y) }
      if(bottomRightBound.y < p.y) { bottomRightBound = Position(bottomRightBound.x, p.y) }

      cells.update(p, value)
    }
  }

  def bounds: (Position, Position) = (topLeftBound, bottomRightBound)
}

object SparseGrid {
  def from[A](lists: List[List[A]], default: A): SparseGrid[A] = {
    val grid = SparseGrid[A](default)

    for(y <- lists.indices) {
      val list = lists(y)
      for(x <- list.indices) {
        grid.update(x, y, list(x))
      }
    }

    grid
  }
}