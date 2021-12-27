object Day24 extends ParseLineDay[Day24.Instruction, Long, Long] {
  def variable: Parser[Variable] = "w" ~> success(Variable.W) | "x" ~> success(Variable.X) | "y" ~> success(Variable.Y) | "z" ~> success(Variable.Z)
  def variableOrNumber: Parser[Variable | Int] = variable | number

  def model: Parser[Instruction] =
        "inp" ~> variable ^^ { Instruction.Inp(_) }
      | "add" ~> variable ~ variableOrNumber ^^ { case l ~ r => Instruction.Add(l, r) }
      | "mul" ~> variable ~ variableOrNumber ^^ { case l ~ r => Instruction.Mul(l, r) }
      | "div" ~> variable ~ variableOrNumber ^^ { case l ~ r => Instruction.Div(l, r) }
      | "mod" ~> variable ~ variableOrNumber ^^ { case l ~ r => Instruction.Mod(l, r) }
      | "eql" ~> variable ~ variableOrNumber ^^ { case l ~ r => Instruction.Eql(l, r) }

  def part1(data: Iterator[Instruction]): Long = solve(data, _.end)
  def part2(data: Iterator[Instruction]): Long = solve(data, _.start)

  def solve(data: Iterator[Instruction], selector: Range => Int): Long = value(generateSpecification(extract(data.toList)), selector)

  /*

  the input is 14 blocks of:

  inp w
  mul x 0
  add x z
  mod x 26
  div z A
  add x B
  eql x w
  eql x 0
  mul y 0
  add y 25
  mul y x
  add y 1
  mul z y
  mul y 0
  add y w
  add y C
  mul y x
  add z y

  where A is 1 or 26, and B & C are variables

  extract those values for later use

  */
  case class Block(pop: Boolean, offset: Int)

  def extract(instructions: List[Instruction]): List[Block] = {
    val numberOfInputs = 14

    instructions.grouped(instructions.length / numberOfInputs).map(i => {
      val pop = (i(4) match { case Instruction.Div(_, v: Int) => v }) match { case 1 => false; case 26 => true }
      val offset = i(if pop then 5 else 15) match { case Instruction.Add(_, v: Int) => v }

      Block(pop, offset)
    }).toList
  }

  /*

  after some careful decompilation of each block, they can be represented as a stack based calculation

  if (pop) {
    head = stack.pop()
    if(head + offset1 != input) {
      stack.push(input + offset2)
    }
  } else {
    stack.push(input + offset1)
  }

  "push" instructions always push, since we want z = 0 (i.e. an empty stack) we want none of the pop instructions to push
  so we can get a relationship between pairs digits and valid ranges they can take

  */
  type InputIndex = Int
  case class InputRelation(source: InputIndex, offset: Int)
  case class InputSpecification(free: Map[InputIndex, Range], related: Map[InputIndex, InputRelation])

  def generateSpecification(blocks: List[Block]): InputSpecification = {
    var stack = List.empty[(InputIndex, Int)]
    var ranges = Map.empty[InputIndex, Range]
    var relations = Map.empty[InputIndex, InputRelation]

    for((block, i) <- blocks.zipWithIndex) {
      if(!block.pop) {
        stack = (i, block.offset) :: stack
      } else {
        val head = stack.head
        stack = stack.tail

        val c = head._2 + block.offset
        val range = ((1 + c).clampLower(1) - c) to ((9 + c).clampUpper(9) - c)

        ranges = ranges.updated(head._1, range)
        relations = relations.updated(i, InputRelation(head._1, c))
      }
    }

    InputSpecification(ranges, relations)
  }

  def generateValues(spec: InputSpecification): List[Long] = {
    val freeDigits = spec.free.foldLeft(List(List.empty[(Int, Int)]))((a, b) => {
      for(l <- a; r <- b._2) yield (b._1, r) :: l
    }).map(_.toMap)
    val allDigits = freeDigits.map(v => v ++ spec.related.map(r => (r._1, v(r._2._1) + r._2._2)))

    allDigits.map(v => (0 to 13).map(v(_)).foldLeft(0L)((a, b) => a * 10 + b))
  }

  def value(spec: InputSpecification, selector: Range => Int): Long = {
    val freeDigits = spec.free.view.mapValues(selector)
    val relatedDigits = spec.related.view.mapValues(r => freeDigits(r._1) + r._2)
    val allDigits = (freeDigits ++ relatedDigits).toMap

    (0 to 13).map(allDigits(_)).foldLeft(0L)((a, b) => a * 10 + b)
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
}
