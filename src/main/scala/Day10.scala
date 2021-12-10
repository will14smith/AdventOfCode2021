import scala.collection.mutable

object Day10 extends LineDay[String, Int, Long] {
  def parseLine(line: String): String = line

  def part1(data: Iterator[String]): Int = data.map(findError).flatMap(_._1).map {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
  }.sum
  def part2(data: Iterator[String]): Long = {
    val scores = data.flatMap(calc).toList.sorted
    scores((scores.size - 1) / 2)
  }

  def calc(line: String): Option[Long] = {
    val (error, stack) = findError(line)

    error match {
      case Some(_) => None
      case None =>
        Some(stack.foldLeft(0L)((a, c) => a * 5 + (c match {
          case ')' => 1
          case ']' => 2
          case '}' => 3
          case '>' => 4
        })))
    }
  }

  def findError(line: String): (Option[Char], mutable.Stack[Char]) = {
    val stack = mutable.Stack[Char]()

    for(c <- line) {
      c match {
        case '(' => stack.push(')')
        case '[' => stack.push(']')
        case '{' => stack.push('}')
        case '<' => stack.push('>')

        case _ => if (stack.pop() != c) { return (Some(c), stack) }
      }
    }

    (None, stack)
  }
}
