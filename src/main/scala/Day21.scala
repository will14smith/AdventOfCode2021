import scala.collection.mutable

object Day21 extends ParseDay[Day21.Game, Int, Long] {
  def player: Parser[Player] = "Player" ~> number ~ ("starting" ~> "position" ~> ":" ~> number) ^^ { case i ~ p => Player(i, p - 1, 0) }
  def model: Parser[Game] = (player <~ "\n") ~ player ^^ { case p1 ~ p2 => Game(0, p1, p2) }

  def part1(initial: Game): Int = {
    var dice = Dice(0, 100)
    var game = initial

    while(game.highScore < 1000) {
      val (numbers, nextDice) = dice.roll(3)
      dice = nextDice
      game = game.play(numbers.sum)
    }

    game.lowScore * game.turn * 3
  }
  def part2(initial: Game): Long = {
    val rolls = (for(d1 <- 1 to 3; d2 <- 1 to 3; d3 <- 1 to 3) yield d1 + d2 + d3).freq

    var universes = mutable.HashMap(initial -> 1L)
    var wins = (0L, 0L)

    while(universes.nonEmpty) {
      val nextUniverses = new mutable.HashMap[Game, Long]
      var newWins = 0L

      for((game, gameFreq) <- universes) {
        for((roll, rollFreq) <- rolls) {
          val nextGame = game.play(roll)

          if(nextGame.highScore >= 21) {
            newWins += gameFreq * rollFreq
          } else {
            nextUniverses.updateWith(nextGame)(f => Some(f.getOrElse(0L) + gameFreq * rollFreq))
          }
        }
      }

      universes = nextUniverses
      wins = (wins._2, wins._1 + newWins)
    }

    Math.max(wins._1, wins._2)
  }

  case class Game(turn: Int, player1: Player, player2: Player) {
    def play(delta: Int): Game = {
      val (p1, p2) = if turn % 2 == 0 then (player1.update(delta), player2) else (player1, player2.update(delta))
      Game(turn + 1, p1, p2)
    }

    def lowScore: Int = Math.min(player1.score, player2.score)
    def highScore: Int = Math.max(player1.score, player2.score)
  }

  case class Player(id: Int, index: Int, score: Int) {
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
