import scala.collection.mutable

object Day10 extends LineDay[String, Int, Long] {
  def parseLine(line: String): String = line

  def part1(data: Iterator[String]): Int = data.map(findError).map(_._1).filter(_.isDefined).map(_.get).map {
    case (')', _) => 3
    case (']', _) => 57
    case ('}', _) => 1197
    case ('>', _) => 25137
  }.sum
  def part2(data: Iterator[String]): Long = {
    val scores = data.map(calc).filter(_.isDefined).map(_.get).toList.sorted

    println(scores)

    scores((scores.size - 1) / 2)
  }

  def calc(line: String): Option[Long] = {
    val (error, stack) = findError(line)

    if(error.isDefined) {
      return None
    }

    var score = 0L

    while(stack.nonEmpty) {
      score = score * 5 + (stack.pop() match {
        case ')' => 1
        case ']' => 2
        case '}' => 3
        case '>' => 4

        case c => {
          println(c)
          ???
        }
      })
    }

    Some(score)
  }

  def findError(line: String): (Option[(Char, Int)], mutable.Stack[Char]) = {
    val stack = mutable.Stack[Char]()

    for(c <- line) {
      c match {
        case '(' => stack.push(')')
        case '[' => stack.push(']')
        case '{' => stack.push('}')
        case '<' => stack.push('>')

        case _ => if (stack.pop() != c) { return (Some((c, 0)), stack) }
      }
    }

    (None, stack)
  }
}
