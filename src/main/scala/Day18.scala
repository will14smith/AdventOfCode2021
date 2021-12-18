import scala.annotation.tailrec

object Day18 extends ParseLineDay[Day18.Number, Int, Int] {
  def regular: Parser[Number] = number ^^ { Number.Regular(_) }
  def pair: Parser[Number] = ("[" ~> model <~ ",") ~ (model <~ "]") ^^ { case l ~ r => Number.Pair(l, r) }
  def model: Parser[Number] = pair | regular

  def part1(data: Iterator[Number]): Int = {
    magnitude(data.reduceLeft(add))
  }
  def part2(data: Iterator[Number]): Int = {
    val nums = data.toList

    val values = for(a <- nums; b <- nums if a != b) yield magnitude(add(a, b))
    values.max
  }

  def add(a: Number, b: Number): Number = reduce(combine(a, b))
  def magnitude(a: Number): Int = {
    a match {
      case Number.Pair(left, right) => 3 * magnitude(left) + 2 * magnitude(right)
      case Number.Regular(value) => value
    }
  }

  def combine(a: Number, b: Number): Number = Number.Pair(a, b)
  def reduce(a: Number): Number = {
    var current = a

    while(true) {
      val (exploded, didExplode) = explode(current)
      if(!didExplode) {
        val (splitted, didSplit) = split(current)
        if(!didSplit) {
          return current
        }

        current = splitted
      } else {
        current = exploded
      }
    }

    current
  }

  def explode(a: Number): (Number, Boolean) = {
    def addRegular(x: Number, y: Number): Number = Number.Regular(x.asInstanceOf[Number.Regular].value + y.asInstanceOf[Number.Regular].value)

    def search(z: Location, depth: Int): (Location, Boolean) = {
      if(depth > 4 && z.focus.isInstanceOf[Number.Pair]) {
        val pair = z.focus.asInstanceOf[Number.Pair]

        val a = z.update(Number.Regular(0))
        val b = a.prev.flatMap(_.update(addRegular(_, pair.left)).next).getOrElse(a)
        val c = b.next.flatMap(_.update(addRegular(_, pair.right)).prev).getOrElse(b)

        return (c, true)
      }

      z.focus match {
        case Number.Pair(_, _) => {
          val first = search(z.first, depth + 1)
          if(first._2) {
            return first
          }

          val second = search(z.second, depth + 1)
          if(second._2) {
            return second
          }

          (z, false)
        }
        case Number.Regular(_) => (z, false)
      }
    }

    search(Location(a, Path.Top), 1).applyLeft(_.upMost.focus)
  }

  def split(a: Number): (Number, Boolean) = {
    a match {
      case Number.Pair(left, right) => {
        val (left1, didSplitLeft) = split(left)
        if (didSplitLeft) {
          return (Number.Pair(left1, right), true)
        }

        val (right1, didSplitRight) = split(right)
        if(didSplitRight) {
          return (Number.Pair(left, right1), true)
        }

        (a, false)
      }
      case Number.Regular(value) if value >= 10 => (Number.Pair(Number.Regular(value / 2), Number.Regular(value - (value / 2))), true)
      case Number.Regular(value) => (a, false)
    }
  }

  enum Number:
    case Pair(left: Number, right: Number)
    case Regular(value: Int)

  enum Path:
    case Top
    case Left(parent: Path, right: Number)
    case Right(left: Number, parent: Path)

  // implements a Zipper (https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf)
  case class Location(focus: Number, path: Path) {
    def update(value: Number): Location = Location(value, path)
    def update(fn: Number => Number): Location = Location(fn(focus), path)

    def tryFirst: Option[Location] = focus match {
      case Number.Pair(left, right) => Some(Location(left, Path.Left(path, right)))
      case Number.Regular(value) => None
    }
    def first: Location = tryFirst.get
    def trySecond: Option[Location] = focus match {
      case Number.Pair(left, right) => Some(Location(right, Path.Right(left, path)))
      case Number.Regular(value) => None
    }
    def second: Location = trySecond.get

    def tryUp: Option[Location] = path match {
      case Path.Top => None
      case Path.Left(parent, right) => Some(Location(Number.Pair(focus, right), parent))
      case Path.Right(left, parent) => Some(Location(Number.Pair(left, focus), parent))
    }
    def up: Location = tryUp.get

    def tryLeft: Option[Location] = path match {
      case Path.Top => None
      case Path.Left(parent, right) => None
      case Path.Right(left, parent) => Some(Location(left, Path.Left(parent, focus)))
    }
    def tryRight: Option[Location] = path match {
      case Path.Top => None
      case Path.Left(parent, right) => Some(Location(right, Path.Right(focus, parent)))
      case Path.Right(left, parent) => None
    }

    def firstMost: Location = tryFirst.map(_.firstMost).getOrElse(this)
    def secondMost: Location = trySecond.map(_.secondMost).getOrElse(this)
    def upMost: Location = tryUp.map(_.upMost).getOrElse(this)

    def prev: Option[Location] = {
      // right most item before current
      // try going left then deep on the seconds
      // otherwise go up a level a try finding the previous there
      tryLeft.map(_.secondMost).orElse(tryUp.flatMap(_.prev))
    }
    def next: Option[Location] = {
      // opposite of prev ^
      tryRight.map(_.firstMost).orElse(tryUp.flatMap(_.next))
    }
  }

  enum Command:
    case Set(path: Path, value: Number)
    case Update(path: Path, value: Number.Regular)
}
