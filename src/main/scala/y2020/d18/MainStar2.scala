package y2020.d18

// scalastyle:off
object MainStar2 extends App {

  val lines = Input.real.split("\n").map(_.trim).filter(_.nonEmpty)
    .map(_.replace(" ", ""))

  lines.foreach(println)



  def parse(line: String)(start: Int): (Long, Int) = {
    var ix = start
    var acc: Long = 1L
    var currentAddition: Long = 0L

    while (ix < line.length) {
      line(ix) match {
        case '(' =>
          val (subExpressionResult, subExpressionEnd) = parse(line)(ix + 1)
          currentAddition = currentAddition + subExpressionResult
          ix = subExpressionEnd
        //println(s"Parens closed, acc: $acc")
        case ')' =>
          acc = acc * currentAddition
          return (acc, ix + 1)
        case '*' =>
          acc = acc * currentAddition
          currentAddition = 0
          ix = ix + 1
        case '+' =>
          ix = ix + 1
        case i =>
          currentAddition = currentAddition + i.toString.toLong
          ix = ix + 1
      }
    }

    acc = acc * currentAddition
    (acc, ix + 1)
  }

  val rs: Array[Long] = lines.map(parse(_)(0)._1)
  rs.foreach(println)
  println(rs.sum)

}
