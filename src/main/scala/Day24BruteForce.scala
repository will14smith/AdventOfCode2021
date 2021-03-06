import scala.collection.mutable
import Day24.{Instruction, Variable}

object Day24BruteForce extends ParseLineDay[Instruction, Long, Long] {
  def model: Parser[Instruction] = Parser(i => Day24.model(i) match {
    case Day24.Success(result, next) => Success(result, next)
    case Day24.Failure(msg, next) => Failure(msg, next)
    case Day24.Error(msg, next) => Error(msg, next)
  })

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
