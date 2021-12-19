import scala.collection.mutable

object Day19 extends ParseDay[Day19.Scanners, Int, Int] {
  def header: Parser[Int] = "---" ~ "scanner" ~> number <~ "---" ~ "\n"
  def beacon: Parser[Position3] = commaNumbers ^^ { case List(x, y, z) => Position3(x, y, z) }
  def scanner: Parser[Scanner] = header ~ rep1sep(beacon, "\n") ^^ { case i ~ b => Scanner(i, b) }

  def model: Parser[Scanners] = repsep(scanner, "\n\n")

  def part1(data: Scanners): Int = {
    val notFound = new mutable.Queue[Scanner]
    var found = List(data.head)

    notFound.enqueueAll(data.tail)

    while(notFound.nonEmpty) {
      val s = notFound.dequeue()
      val r = find(found, s)
      if(r.isDefined) {
        found = r.get._1 :: found
      } else {
        notFound.enqueue(s)
      }
    }

    found.flatMap(_.beacons).toSet.size
  }
  def part2(data: Scanners): Int = {
    val notFound = new mutable.Queue[Scanner]
    var found = List(data.head)
    var offsets = List(Position3.identity)

    notFound.enqueueAll(data.tail)

    while(notFound.nonEmpty) {
      val s = notFound.dequeue()
      val r = find(found, s)
      if(r.isDefined) {
        found = r.get._1 :: found
        offsets = r.get._2 :: offsets
      } else {
        notFound.enqueue(s)
      }
    }

    (for(a <- offsets; b <- offsets) yield ((a.x - b.x).abs + (a.y - b.y).abs + (a.z - b.z).abs)).max
  }

  def find(k: List[Scanner], s: Scanner): Option[(Scanner, Position3)] = {
    for(r <- Matrix33.axisRotations) {
      val sr = transform(s, r)

      for (sk <- k) {
        val distanceVectors = vectors(sk, sr)
        val matches = distanceVectors.freq.filter(_._2 >= 12)
        if (matches.nonEmpty) {
          val offset = matches.head._1
          val srt = translate(sr, offset)
          return Some((srt, offset))
        }
      }
    }

    None
  }

  def vectors(a: Day19.Scanner, b: Day19.Scanner): List[Position3] = {
    val result = List.newBuilder[Position3]

    for(x <- a.beacons; y <- b.beacons) {
      result.addOne(x - y)
    }

    result.result()
  }

  def transform(scanner: Scanner, delta: Matrix33): Scanner = Scanner(scanner.id, scanner.beacons.map(delta * _))
  def translate(scanner: Scanner, delta: Position3): Scanner = Scanner(scanner.id, scanner.beacons.map(_ + delta))
  def overlap(a: Day19.Scanner, b: Day19.Scanner) = a.beacons.count(b.beacons.contains)

  case class Scanner(id: Int, beacons: List[Position3])
  type Scanners = List[Scanner]
}
