object Day21 extends ParseLineDay[Day21.Player, Int, Long] {
  def model: Parser[Player] = "Player" ~> number ~ ("starting" ~> "position" ~> ":" ~> number) ^^ { case i ~ p => Player(i, p - 1, 0) }

  def part1(data: Iterator[Player]): Int = {
    var dice = Dice(0, 100)
    var players = data.toIndexedSeq

    var turn = 0

    while(true) {
      val playerIndex = turn % players.size
      val player = players(playerIndex)
      val (nextDice, nextPlayer) = player.roll(dice, 3)

      turn += 1
      players = players.updated(playerIndex, nextPlayer)
      dice = nextDice

      if(nextPlayer.score >= 1000) {
        return players(turn % players.size).score * turn * 3
      }
    }

    ???
  }
  def part2(data: Iterator[Player]): Long = {
    val rolls = (for(d1 <- 1 to 3; d2 <- 1 to 3; d3 <- 1 to 3) yield d1 + d2 + d3).freq

    val ps = data.toList

    var universes = Map((ps.head, ps.last) -> 1L)
    var wins = ps.map(p => p.id -> 0L).toMap

    var turn = 0

    while(universes.nonEmpty) {
      val playerTurn = (turn % 2) + 1

      val (wonUniverses, nextUniverses) = universes.freqFlatMapWithFreq(universe => {
        val (p1, p2) = universe

        rolls.map(rf => ((if playerTurn == 1 then p1.update(rf._1) else p1, if playerTurn == 2 then p2.update(rf._1) else p2), rf._2))
      }).partition(p => (if playerTurn == 1 then p._1._1 else p._1._2).score >= 21)

      universes = nextUniverses
      wins = wins.updatedWith(playerTurn)(n => Some(n.getOrElse(0L) + wonUniverses.values.sum))

      turn += 1
    }

    wins.values.max
  }

  case class Player(id: Int, index: Int, score: Int) {
    def roll(dice: Dice, n: Int): (Dice, Player) = {
      val (numbers, nextDice) = dice.roll(n)
      val next = update(numbers.sum)

      (nextDice, next)
    }

    def update(delta: Int): Player = {
      val nextIndex = (index + delta) % 10
      Player(id, nextIndex, score + nextIndex + 1)
    }

  }
  case class Dice(index: Int, sides: Int) {
    def roll(n: Int): (Seq[Int], Dice) = {
      val numbers = for(i <- 0 until n) yield ((i + index) % sides) + 1
      val next = Dice((index + n) % sides, sides)

      (numbers, next)
    }
  }
}
