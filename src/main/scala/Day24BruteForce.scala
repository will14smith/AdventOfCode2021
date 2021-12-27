import scala.collection.mutable

object Day24BruteForce extends ParseLineDay[Day24BruteForce.Instruction, Long, Long] {
  def variable: Parser[Variable] = "w" ~> success(Variable.W) | "x" ~> success(Variable.X) | "y" ~> success(Variable.Y) | "z" ~> success(Variable.Z)
  def variableOrNumber: Parser[Variable | Int] = variable | number

  def model: Parser[Instruction] =
        "inp" ~> variable ^^ { Instruction.Inp(_) }
      | "add" ~> variable ~ variableOrNumber ^^ { case l ~ r => Instruction.Add(l, r) }
      | "mul" ~> variable ~ variableOrNumber ^^ { case l ~ r => Instruction.Mul(l, r) }
      | "div" ~> variable ~ variableOrNumber ^^ { case l ~ r => Instruction.Div(l, r) }
      | "mod" ~> variable ~ variableOrNumber ^^ { case l ~ r => Instruction.Mod(l, r) }
      | "eql" ~> variable ~ variableOrNumber ^^ { case l ~ r => Instruction.Eql(l, r) }

  def part1(data: Iterator[Instruction]): Long = solve(data.toList, 9 to 1 by -1)
  def part2(data: Iterator[Instruction]): Long = solve(data.toList, 1 to 9)

  def solve(instructions: List[Instruction], digits: Seq[Int]): Long = {
    val visited = new mutable.HashSet[(State, Int)]()

    def eval(state: State, instructions: List[Instruction], depth: Int, input: Long): Option[Long] = {
      if(instructions.isEmpty) {
        return if state.z == 0 then Some(input) else None
      }

      def get(p: Variable | Int): Long = p match {
        case v: Variable => state.get(v)
        case v: Int => v
      }

      instructions.head match {
        case Instruction.Inp(variable) =>
          for(n <- digits) {
            val next = state.set(variable, n)
            if(visited.add((next, depth))) {
              val result = eval(next, instructions.tail, depth + 1, input * 10 + n)
              if(result.isDefined) return result
            }
          }

          None
        case Instruction.Add(left, right) =>
          val next = state.set(left, state.get(left) + get(right))
          eval(next, instructions.tail, depth, input)
        case Instruction.Mul(left, right) =>
          val next = state.set(left, state.get(left) * get(right))
          eval(next, instructions.tail, depth, input)
        case Instruction.Div(left, right) =>
          val next = state.set(left, state.get(left) / get(right))
          eval(next, instructions.tail, depth, input)
        case Instruction.Mod(left, right) =>
          val next = state.set(left, state.get(left) % get(right))
          eval(next, instructions.tail, depth, input)
        case Instruction.Eql(left, right) =>
          val next = state.set(left, if state.get(left) == get(right) then 1 else 0)
          eval(next, instructions.tail, depth, input)
      }
    }

    eval(State.initial, instructions, 0, 0).get
  }

  enum Variable:
    case W
    case X
    case Y
    case Z
  enum Instruction:
    case Inp(variable: Variable)
    case Add(left: Variable, right: Variable | Int)
    case Mul(left: Variable, right: Variable | Int)
    case Div(left: Variable, right: Variable | Int)
    case Mod(left: Variable, right: Variable | Int)
    case Eql(left: Variable, right: Variable | Int)

  case class State(w: Long, x: Long, y: Long, z: Long) {
    def get(source: Variable): Long = {
      source match {
        case Variable.W => w
        case Variable.X => x
        case Variable.Y => y
        case Variable.Z => z
      }
    }
    def set(target: Variable, value: Long): State = {
      target match {
        case Variable.W => copy(w = value)
        case Variable.X => copy(x = value)
        case Variable.Y => copy(y = value)
        case Variable.Z => copy(z = value)
      }
    }
  }
  object State {
    def initial: State = State(0, 0, 0, 0)
  }
}
