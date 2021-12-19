import scala.collection.mutable

object Day19 extends ParseDay[Day19.Scanners, Int, Int] {
  def header: Parser[Int] = "---" ~ "scanner" ~> number <~ "---" ~ "\n"
  def beacon: Parser[Position3] = commaNumbers ^^ { case List(x, y, z) => Position3(x, y, z) }
  def scanner: Parser[Scanner] = header ~ rep1sep(beacon, "\n") ^^ { case i ~ b => Scanner(i, b) }

  def model: Parser[Scanners] = repsep(scanner, "\n\n")

  def part1(data: Scanners): Int = findAll(data).map(_._1).flatMap(_.beacons).toSet.size
  def part2(data: Scanners): Int = {
    val offsets = findAll(data).map(_._2)
    (for(a <- offsets; b <- offsets) yield a.manhattan(b)).max
  }

  def findAll(input: Scanners): List[(Scanner, Position3)] = findAllWithMatches(input, findMatches(input))

  def findMatches(input: Scanners): List[(Int, Int)] = {
    val scanners = input.map(s => (s.id, calculateDistances(s)))

    var matches = List.empty[(Int, Int)]

    for (ia <- 0 until scanners.size; ib <- (ia + 1) until scanners.size) {
      val a = scanners(ia)
      val b = scanners(ib)

      if(a._2.intersect(b._2).size >= 12*11) {
        matches = (ia, ib) :: matches
      }
    }

    matches
  }


  def findAllWithMatches(input: Scanners, matches: List[(Int, Int)]): List[(Scanner, Position3)] = {
    val notFound = new mutable.Queue[Scanner]
    var found = List((input.head, Position3.identity))

    notFound.enqueueAll(input.tail)

    while(notFound.nonEmpty) {
      val s = notFound.dequeue()

      val m = matches.filter(_._1 == s.id).map(_._2) ++ matches.filter(_._2 == s.id).map(_._1)

      val foundMatches = found.filter(a => m.contains(a._1.id)).map(_._1)

      val r = find(foundMatches, s)
      if(r.isDefined) {
        found = r.get :: found
      } else {
        notFound.enqueue(s)
      }
    }

    found
  }

  def calculateDistances(scanner: Scanner): List[Int] = {
    for (x <- scanner.beacons; y <- scanner.beacons if x != y) yield x.manhattan(y)
  }

  def find(known: List[Scanner], scanner: Scanner): Option[(Scanner, Position3)] = {
    for(r <- Matrix33.axisRotations) {
      val rotatedScanner = transform(scanner, r)

      for (knownScanner <- known) {
        val offsetOpt = findOffset(knownScanner, rotatedScanner)

        if (offsetOpt.isDefined) {
          val offset = offsetOpt.get
          val rotatedOffsetScanner = translate(rotatedScanner, offset)
          return Some((rotatedOffsetScanner, offset))
        }
      }
    }

    None
  }

  def findOffset(a: Scanner, b: Scanner): Option[Position3] = {
    val map = new mutable.HashMap[Position3, Long](a.beacons.size*b.beacons.size, 0.75)

    for(x <- a.beacons; y <- b.beacons) {
      val v = x - y

      val r = map.updateWith(v)(n => n.map(_ + 1).orElse(Some(1)))
      if(r.get >= 12) {
        return Some(v)
      }
    }

    None
  }

  def transform(scanner: Scanner, delta: Matrix33): Scanner = Scanner(scanner.id, scanner.beacons.map(delta * _))
  def translate(scanner: Scanner, delta: Position3): Scanner = Scanner(scanner.id, scanner.beacons.map(_ + delta))

  case class Scanner(id: Int, beacons: List[Position3])
  type Scanners = List[Scanner]
}
