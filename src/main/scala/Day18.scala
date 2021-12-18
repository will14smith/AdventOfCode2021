import scala.annotation.tailrec

object Day18 extends ParseLineDay[Day18.Number, Int, Int] {
  def regular: Parser[Number] = number ^^ { Number.Regular(_) }
  def pair: Parser[Number] = (("[" ~> model) <~ ",") ~ (model <~ "]") ^^ { case l ~ r => Number.Pair(l, r) }
  def model: Parser[Number] = pair | regular

  def part1(data: Iterator[Number]): Int = {
    magnitude(data.reduceLeft(add))
  }
  def part2(data: Iterator[Number]): Int = {
    val l = data.toList

    var max = 0

    for(a <- l; b <- l) {
      if(a != b) {
        max = Math.max(max, magnitude(add(a, b)))
        max = Math.max(max, magnitude(add(b, a)))
      }
    }

    max
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
      val exploded = explode(current)
      if(exploded == current) {
        val splitted = split(current)
        if(splitted == current) {
          return current
        }

        current = splitted
      } else {
        current = exploded
      }
    }

    current
  }

  def explode(a: Number): Number = {
    def inner(n: Number, path: Path): List[Command] = {
      n match {
        case Number.Pair(left, right) => {
          if(path.length >= 4) {
            return List(
              Command.Left(path.appended(false), left.asInstanceOf[Number.Regular]),
              Command.Right(path.appended(true), right.asInstanceOf[Number.Regular]),
              Command.Set(path, Number.Regular(0)),
            )
          }

          val leftCommands = inner(left, path.appended(false))
          if(leftCommands.nonEmpty) {
            return leftCommands
          }

          inner(right, path.appended(true))
        }
        case Number.Regular(value) => List()
      }
    }

    def apply(a: Number, command: Command): Number = {
      val ps = paths(a)

      command match {
        case Command.Set(path, value) => if path.isEmpty then value else a match {
          case Number.Pair(left, right) => Number.Pair(if path.head then left else apply(left, Command.Set(path.tail, value)), if path.head then apply(right, Command.Set(path.tail, value)) else right)
          case Number.Regular(value) => ???
        }
        case Command.Update(path, value) => a match {
          case Number.Pair(left, right) => Number.Pair(if path.head then left else apply(left, Command.Update(path.tail, value)), if path.head then apply(right, Command.Update(path.tail, value)) else right)
          case Number.Regular(current) => if path.isEmpty then Number.Regular(current + value.value) else ???
        }
        case Command.Left(path, value) => {
          val index = ps.indexOf(path)
          if index == 0 then a else apply(a, Command.Update(ps(index - 1), value))
        }
        case Command.Right(path, value) => {
          val index = ps.indexOf(path)
          if index + 1 == ps.length then a else apply(a, Command.Update(ps(index + 1), value))
        }
      }
    }

    val commands = inner(a, List())
    commands.foldLeft(a)(apply)
  }

  def split(a: Number): Number = {
    a match {
      case Number.Pair(left, right) => {
        val left1 = split(left)
        if (left1 != left) {
          return Number.Pair(left1, right)
        }

        val right1 = split(right)
        Number.Pair(left, right1)
      }
      case Number.Regular(value) if value >= 10 => Number.Pair(Number.Regular(value / 2), Number.Regular(value - (value / 2)))
      case Number.Regular(value) => a
    }
  }

  def paths(a: Number, path: Path = List()): List[Path] = {
    a match {
      case Number.Pair(left, right) => paths(left, path.appended(false)) ++ paths(right, path.appended(true))
      case Number.Regular(value) => List(path)
    }
  }

  enum Number:
    case Pair(left: Number, right: Number)
    case Regular(value: Int)

  type Path = List[Boolean]

  enum Command:
    case Set(path: Path, value: Number)
    case Update(path: Path, value: Number.Regular)

    case Left(path: Path, value: Number.Regular)
    case Right(path: Path, value: Number.Regular)
}
