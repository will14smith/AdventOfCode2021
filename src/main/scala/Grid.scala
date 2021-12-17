import scala.io.AnsiColor.{RED, RESET}
import scala.reflect.ClassTag

case class Position(x: Int, y: Int) {
  def neighbours: Seq[Position] = for(dy <- -1 to 1; dx <- -1 to 1) yield Position(x + dx, y + dy)
  def orthogonal: Seq[Position] = Seq(copy(x = x + 1), copy(x = x - 1), copy(y = y + 1), copy(y = y - 1))

  def +(other: Position): Position = Position(x + other.x, y + other.y)
}

case class Grid[T](private val elements: Array[Array[T]]) {
  def apply(p: Position): T = apply(p.x, p.y)
  def apply(x: Int, y: Int): T = elements(y)(x)
  def update(p: Position, value: T): Unit = update(p.x, p.y, value)
  def update(x: Int, y: Int, value: T): Unit = elements(y).update(x, value)
  def map[B](f: T => B)(implicit ct: ClassTag[B]): Grid[B] = Grid(elements.map(_.map(f)))
  def map[B](f: (T, Position) => B)(implicit ct: ClassTag[B]): Grid[B] = {
    val newElements = Array.ofDim[B](h, w)

    for { y <- 0 until h; x <- 0 until w } {
      newElements(y)(x) = f(elements(y)(x), Position(x, y))
    }

    Grid(newElements)
  }

  def keys: Seq[Position] = for { y <- 0 until h; x <- 0 until w } yield Position(x, y)

  def w: Int = if h == 0 then 0 else elements(0).length
  def h: Int = elements.length

  def isValid(p: Position): Boolean = isValid(p.x, p.y)
  def isValid(x: Int, y: Int): Boolean = 0 <= y && y < h && 0 <= x && x < w
}

object Grid {
  def empty[T](g: Grid[T])(implicit ct: ClassTag[T]): Grid[T] = empty[T](g.w, g.h)
  def empty[T](w: Int, h: Int)(implicit ct: ClassTag[T]): Grid[T] = Grid(Array.ofDim[T](h, w))
}

extension[A] (g: Grid[A])
  def debug(highlight: Position => Boolean): Unit = debug(highlight, _.toString)

  def debug(highlight: Position => Boolean, show: A => String): Unit = {
    for (y <- 0 until g.h) {
      for (x <- 0 until g.w) {
        if (highlight(Position(x, y))) print(RED)
        print(show(g(x, y)))
        print(RESET)
      }
      
      println()
    }
  }