package y2020.d18

import scala.collection.mutable

// scalastyle:off
object MainStar1 extends App {

  val lines = Input.real.split("\n").map(_.trim).filter(_.nonEmpty)
    .map(_.replace(" ", ""))

  lines.foreach(println)

  def parse(line: String)(start: Int): (Long, Int) = {
    var ix = start
    var acc = 0L
    var operation: Operator = Plus

    while (ix < line.length) {
      line(ix) match {
        case '(' =>
          val (subExpressionResult, subExpressionEnd) = parse(line)(ix + 1)
          ix = subExpressionEnd
          acc = operation(acc, subExpressionResult)
          //println(s"Parens closed, acc: $acc")
        case ')' =>
          return (acc, ix + 1)
        case '+' =>
          operation = Plus
          ix = ix + 1
        case '*' =>
          operation = Mult
          ix = ix + 1
        case i =>
          acc = operation(acc, i.toString.toLong)
          //println(s"Number applied, acc: $acc")
          ix = ix + 1
      }
    }
    (acc, ix + 1)
  }

  val rs: Array[Long] = lines.map(parse(_)(0)._1)
  rs.foreach(println)
  println(rs.sum)

}
