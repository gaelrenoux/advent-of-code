package y2020.d19

// scalastyle:off
object MainStar1 extends App {

  val Array(rulesTxt, linesTxt) = Input.real.split("\n\n")

  val ruleDefinitions = rulesTxt.split("\n").map(_.trim).filter(_.nonEmpty).map { ruleLine =>
    val Array(ruleIx, ruleContent) = ruleLine.split(":")
    ruleIx.toInt -> RuleDef.parse(ruleContent.trim)
  }.sortBy(_._1).toMap

  val lines = linesTxt.split("\n").map(_.trim).filter(_.nonEmpty)

  val ruleZero = new Rule.Builder(ruleDefinitions).getRule(0)

  ruleDefinitions.toSeq.sortBy(_._1).foreach(println)
  println(ruleZero.toFullRegex)

  lines.foreach { line =>
    println(s"$line: ${ruleZero.matches(line)}")
  }

  val r = lines.count(ruleZero.matches)
  println(r)

}
