import Day24.{Instruction, Variable}

import scala.collection.mutable

object Day24Symbolic  extends ParseLineDay[Instruction, Long, Long] {
  def model: Parser[Instruction] = Parser(i => Day24.model(i) match {
    case Day24.Success(result, next) => Success(result, next)
    case Day24.Failure(msg, next) => Failure(msg, next)
    case Day24.Error(msg, next) => Error(msg, next)
  })

  def part1(data: Iterator[Instruction]): Long = build(data).max
  def part2(data: Iterator[Instruction]): Long = build(data).min

  def build(data: Iterator[Instruction]): Seq[Long] = {
    val instructions = data.toList
    val initial = Variable.values.map((_, Value(0))).toMap[Variable, Symbolic]

    val results = build_rec(instructions, initial, List.empty)

    results.map(r => {
      r.assignments.iterator.toList.sortBy(x => x._1.name.substring("input".length).toInt).map(_._2).foldLeft(0L)((a, x) => a * 10 + x)
    })
  }

  def build_rec(instructions: List[Instruction], registers: Map[Variable, Symbolic], inputs: List[Symbol]): Seq[EnumerationResult] = {
    def get(v: Variable | Int): Symbolic = v match {
      case v: Variable => registers(v)
      case v: Int => Value(v)
    }

    if(instructions.isEmpty) {
      // only return valid results
      return registers(Variable.Z).enumerate.filter(_.value == 0)
    }

    val instruction = instructions.head

    val target = instruction match {
      case Instruction.Inp(variable) => variable
      case Instruction.Add(left, right) => left
      case Instruction.Mul(left, right) => left
      case Instruction.Div(left, right) => left
      case Instruction.Mod(left, right) => left
      case Instruction.Eql(left, right) => left
    }

    var nextInputs = inputs

    val value = instruction match {
      case Instruction.Inp(variable) =>
        val input = Symbol(s"input${inputs.size + 1}", (1 to 9).map(_.toLong))
        nextInputs = nextInputs.appended(input)
        input
      case Instruction.Add(left, right) => get(left) + get(right)
      case Instruction.Mul(left, right) => get(left) * get(right)
      case Instruction.Div(left, right) => get(left) / get(right)
      case Instruction.Mod(left, right) => get(left) % get(right)
      case Instruction.Eql(left, right) => {
        // eql instructions are the only places where we can symbolically branch
        // through state pruning we can avoid the 2^28 branches that would be possible
        val expr = (get(left) - get(right)).simplify._1
        val rawValues = expr.enumerate.toIndexedSeq
        val values = prune(rawValues)

        val (equalSolutions, nonEqualSolutions) = values.partition(_.value == 0)

        val equalRec = if equalSolutions.isEmpty then Seq.empty else build_rec(instructions.tail, registers.updated(target, Value(1)), nextInputs).distinct
        val nonEqualRec = if nonEqualSolutions.isEmpty then Seq.empty else build_rec(instructions.tail, registers.updated(target, Value(0)), nextInputs).distinct

        return EnumerationResult.combineAll(equalSolutions, equalRec, _ + _) ++ EnumerationResult.combineAll(nonEqualSolutions, nonEqualRec, _ + _)
      }
    }

    val nextRegisters = registers.updated(target, value.simplify._1)

    build_rec(instructions.tail, nextRegisters, nextInputs)
  }

  def prune(input: IndexedSeq[EnumerationResult]): IndexedSeq[EnumerationResult] = {
    val symbols = input.flatMap(_.assignments.keys).distinct

    var unused = List.empty[Symbol]

    for(symbol <- symbols) {
      val a = input.map(i => {
        EnumerationResult(i.assignments - symbol, i.value)
      }).groupMap(_.assignments)(_.value).view.mapValues(x => x.distinct.size)

      if(a.values.forall(_ == 1)) {
        unused = unused.appended(symbol)
      }
    }

    input.map(i => EnumerationResult(i.assignments -- unused, i.value)).distinct
  }

  case class EnumerationResult(assignments: mutable.Map[Symbol, Long], value: Long) {
    def map(fn: Long => Long): EnumerationResult = EnumerationResult(assignments, fn(value))
    def combine(other: EnumerationResult, fn: (Long, Long) => Long): Option[EnumerationResult] = {
      val combined = new mutable.HashMap[Symbol, Long](assignments.size + other.assignments.size, 1)

      combined.addAll(assignments)
      for((s, v) <- other.assignments) {
        val existing = combined.getOrElseUpdate(s, v)
        if (existing != v) {
          return None
        }
      }

      Some(EnumerationResult(combined, fn(value, other.value)))
    }
  }
  object EnumerationResult {
    def value(v: Long): EnumerationResult = EnumerationResult(mutable.Map.empty, v)
    def assign(s: Symbol, v: Long): EnumerationResult = EnumerationResult(mutable.HashMap.from(Seq(s -> v)), v)

    def combineAll(as: Seq[EnumerationResult], bs: Seq[EnumerationResult], fn: (Long, Long) => Long): Seq[EnumerationResult] = as.flatMap(a => bs.flatMap(b => a.combine(b, fn)))
  }

  trait Symbolic {
    def +(other: Symbolic): Symbolic = Addition(this, other)
    def unary_- : Symbolic = Negate(this)
    def -(other: Symbolic): Symbolic = Addition(this, -other)
    def *(other: Symbolic): Symbolic = Multiplication(this, other)
    def /(other: Symbolic): Symbolic = Division(this, other)
    def %(other: Symbolic): Symbolic = Modulus(this, other)

    def enumerate: Seq[EnumerationResult]

    def simplify: (Symbolic, Boolean) = {
      def simplifyBinary(cons: (left: Symbolic, right: Symbolic) => Symbolic, left: Symbolic, right: Symbolic) = {
        val leftSimplified = left.simplify
        val rightSimplified = right.simplify

        if(leftSimplified._2 || rightSimplified._2) {
          cons(leftSimplified._1, rightSimplified._1).simplify.applyRight(_ => true)
        } else {
          (this, false)
        }
      }

      def countSymbols(s: Symbolic): Int = s match {
        case _: Value => 0
        case _: Symbol => 1
        case Negate(inner) => countSymbols(inner)
        case Addition(left, right) => countSymbols(left) + countSymbols(right)
        case Multiplication(left, right) => countSymbols(left) + countSymbols(right)
        case Division(left, right) => countSymbols(left) + countSymbols(right)
        case Modulus(left, right) => countSymbols(left) + countSymbols(right)
      }
      def isSimple(s: Symbolic) = countSymbols(s) <= 1

      this match {
        case Negate(Value(inner)) => (Value(-inner), true)
        case Negate(Negate(inner)) => inner.simplify.applyRight(_ => true)
        case Negate(inner) =>
          val innerSimplified = inner.simplify
          if(innerSimplified._2) {
            Negate(innerSimplified._1).simplify.applyRight(_ => true)
          } else {
            (this, false)
          }

        case Addition(Value(a), Value(b)) => (Value(a + b), true)
        case Addition(Addition(left, Value(a)), Value(b)) => Addition(left, Value(a + b)).simplify
        case Addition(x, Value(0)) => x.simplify.applyRight(_ => true)
        case Addition(Value(0), x) => x.simplify.applyRight(_ => true)
        case Addition(x, y) => simplifyBinary(Addition.apply, x, y)

        case Multiplication(Value(a), Value(b)) => (Value(a * b), true)
        case Multiplication(x, Value(0)) => (Value(0), true)
        case Multiplication(Value(0), x) => (Value(0), true)
        case Multiplication(x, Value(1)) => x.simplify.applyRight(_ => true)
        case Multiplication(Value(1), x) => x.simplify.applyRight(_ => true)
        case Multiplication(x, y) => simplifyBinary(Multiplication.apply, x, y)

        case Division(Value(a), Value(b)) => (Value(a / b), true)
        case Division(Value(0), x) => (Value(0), true)
        case Division(x, Value(1)) => x.simplify.applyRight(_ => true)
        case Division(left, Value(a)) if isSimple(left) =>
          if left.enumerate.forall(_.value.between(-a + 1, a - 1)) then (Value(0), true) else (this, false)
        // (ax+b) / a == x + b/a
        case Division(Addition(Multiplication(left, Value(m)), right), Value(d)) if m == d => Addition(left, Division(right, Value(d))).simplify.applyRight(_ => true)
        case Division(x, y) => simplifyBinary(Division.apply, x, y)

        case Modulus(Value(a), Value(b)) => (Value(a % b), true)
        case Modulus(Modulus(x, y), z) if y == z => (Modulus(x, y), true)
        case Modulus(Multiplication(x, y), z) if y == z => (Value(0), true)
        case Modulus(left, Value(a)) if isSimple(left) =>
          if left.enumerate.forall(_.value.between(0, a - 1)) then left.simplify.applyRight(_ => true) else (this, false)
        case Modulus(Addition(x, y), b) =>
          val l = Modulus(x, b).simplify
          val r = Modulus(y, b).simplify

          val lc = l._2 && l._1 != x
          val rc = r._2 && r._1 != y

          if(lc || rc) {
            (Modulus(Addition(l._1, r._1).simplify._1, b), true)
          } else {
            (this, false)
          }
        case Modulus(x, y) => simplifyBinary(Modulus.apply, x, y)

        case x => (x, false)
      }
    }
  }

  case class Value(value: Long) extends Symbolic {
    def enumerate = Seq(EnumerationResult.value(value))
  }
  case class Symbol(name: String, values: Seq[Long]) extends Symbolic {
    def enumerate = values.map(v => EnumerationResult.assign(this, v))
  }
  case class Negate(inner: Symbolic) extends Symbolic {
    def enumerate = inner.enumerate.map(_.map(-_))
  }
  case class Addition(left: Symbolic, right: Symbolic) extends Symbolic {
    def enumerate = EnumerationResult.combineAll(left.enumerate, right.enumerate, _ + _)
  }
  case class Multiplication(left: Symbolic, right: Symbolic) extends Symbolic {
    def enumerate = EnumerationResult.combineAll(left.enumerate, right.enumerate, _ * _)
  }
  case class Division(left: Symbolic, right: Symbolic) extends Symbolic {
    def enumerate = EnumerationResult.combineAll(left.enumerate, right.enumerate, _ / _)
  }
  case class Modulus(left: Symbolic, right: Symbolic) extends Symbolic {
    def enumerate = EnumerationResult.combineAll(left.enumerate, right.enumerate, _ % _)
  }
}
