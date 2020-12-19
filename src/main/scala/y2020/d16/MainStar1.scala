package y2020.d16

// scalastyle:off
object MainStar1 extends App {

  val RuleRegex = """([^:]*): (\d+)-(\d+) or (\d+)-(\d+)""".r
  type Ticket = Array[Int]

  val input = Input.real

  val Array(rulesTxt, myTicketsTxt, otherTicketsTxt) = input.split("\n\n")
  val ruleLines = rulesTxt.split("\n").map(_.trim).filter(_.nonEmpty)
  val rules = ruleLines.map {
    case RuleRegex(name, min1, max1, min2, max2) => Field(name, min1.toInt, max1.toInt, min2.toInt, max2.toInt)
  }.toList

  println(rules)

  val otherTickets: Array[Ticket] = otherTicketsTxt.split("\n").map(_.trim).filter(_.nonEmpty).tail.map(_.split(",").map(_.toInt))
  println(otherTickets.toList.map(_.toList))

  val result = otherTickets.view.flatten.filterNot(t => rules.exists(_.isValid(t))).sum

  println(result)


}
