object Day16 extends Day[Day16.Packet, Int, Long] {
  def parse(input: String): Packet = parsePacket(input.flatMap(hexToBinary))._1

  def parsePacket(bits: String): (Packet, String) = {
    val version = binaryToInt(bits.slice(0, 3))
    val pType =  binaryToInt(bits.slice(3, 6))

    val remaining = bits.substring(6)

    pType match {
      case 4 => parseLiteral(remaining, version)
      case _ => parseOperator(remaining, version, pType)
    }
  }
  def parseLiteral(bits: String, version: Int): (Packet, String) = {
    var value = 0L
    var remaining = bits

    var shouldStop = false
    while(!shouldStop) {
      value = (value << 4) | binaryToInt(remaining.slice(1, 5))

      if(remaining.head == '0') {
        shouldStop = true
      }

      remaining = remaining.substring(5)
    }

    (Packet.Literal(version, value), remaining)
  }
  def parseOperator(bits: String, version: Int, operator: Int): (Packet, String) = {
    val lengthType = bits.head
    val lengthLength = if lengthType == '0' then 15 else 11
    val length = binaryToInt(bits.slice(1, lengthLength + 1))
    var remaining = bits.substring(lengthLength + 1)

    var packets = List[Packet]()

    if lengthType == '0' then {
      val endLength = bits.length - 1 - lengthLength - length
      while(remaining.length > endLength) {
        val result = parsePacket(remaining)

        packets = packets.appended(result._1)
        remaining = result._2
      }
    } else {
      for(i <- 1 to length) {
        val result = parsePacket(remaining)

        packets = packets.appended(result._1)
        remaining = result._2
      }
    }

    (Packet.Operator(version, operator, packets), remaining)
  }

  def hexToBinary(h: Char): String = h match {
    case '0' => "0000"
    case '1' => "0001"
    case '2' => "0010"
    case '3' => "0011"
    case '4' => "0100"
    case '5' => "0101"
    case '6' => "0110"
    case '7' => "0111"
    case '8' => "1000"
    case '9' => "1001"
    case 'A' => "1010"
    case 'B' => "1011"
    case 'C' => "1100"
    case 'D' => "1101"
    case 'E' => "1110"
    case 'F' => "1111"
  }
  def binaryToInt(b: String): Int = Integer.parseInt(b, 2)

  def part1(data: Packet): Int = versions(data).sum
  def part2(data: Packet): Long = eval(data)

  def versions(data: Packet): List[Int] = data match {
    case Packet.Literal(version, _) => List(version)
    case Packet.Operator(version, _, packets) => version :: packets.flatMap(versions)
  }

  def eval(data: Packet): Long = {
    data match {
      case Packet.Literal(_, value) => value
      case Packet.Operator(_, operator, packets) => evalOperator(operator, packets.map(eval))
    }
  }
  def evalOperator(operator: Int, values: List[Long]): Long = operator match {
    case 0 => values.sum
    case 1 => values.product
    case 2 => values.min
    case 3 => values.max
    case 5 => evalBoolean(_ > _)(values)
    case 6 => evalBoolean(_ < _)(values)
    case 7 => evalBoolean(_ == _)(values)
  }
  def evalBoolean(fn: (Long, Long) => Boolean)(values: List[Long]): Long = values match { case List(a, b) => if fn(a, b) then 1 else 0 }

  enum Packet(version: Int):
    case Literal(version: Int, value: Long) extends Packet(version)
    case Operator(version: Int, operator: Int, packets: List[Packet]) extends Packet(version)
}
