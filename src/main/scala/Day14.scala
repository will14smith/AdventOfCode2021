object Day14 extends ParseDay[Day14.Model, Long, Long] {
  def template: Parser[String] = "[A-Z]+".r
  def rule: Parser[(String, String)] = "[A-Z]{2}".r ~ ("->" ~> "[A-Z]".r) ^^ { case a ~ b => (a, b) }
  def rules: Parser[Map[String, String]] = rep1sep(rule, "\n") ^^ { _.toMap }
  def model: Parser[Model] = template ~ ("\n\n" ~> rules) ^^ { case t ~ r => Model(t, r) }

  def part1(data: Model): Long = solve(data, 10)
  def part2(data: Model): Long = solve(data, 40)

  def solve(data: Model, iterations: Int): Long = count(Seq.iterate(data.templatePairs, iterations + 1)(data.applyRules).last)

  def count(pairs: Map[String, Long]): Long = {
    val letters = pairs.freqFlatMap(s => s.iterator)
    (letters.values.max / 2) - (letters.values.min / 2)
  }

  case class Model(template: String, rules: Map[String, String]) {
    def templatePairs: Map[String, Long] = template.sliding(2).concat(List(s"${template.head}", s"${template.last}")).freq

    def applyRules(pairs: Map[String, Long]): Map[String, Long] = pairs.freqFlatMap(s =>
      rules.get(s) match {
        case Some(b) => List(s"${s.head}$b", s"$b${s.tail}")
        case None => List(s)
      })
  }
}
