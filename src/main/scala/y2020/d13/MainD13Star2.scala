package y2020.d13

import y2020.log

// scalastyle:off
object MainD13Star2 extends App {

  val text = Input.real
  val lines = text.split("\n").map(_.trim).filter(_.nonEmpty)
  val schedules = lines(1).split(",").zipWithIndex.flatMap {
    case ("x", _) => None
    case (txt, i) => Some(Bus(txt.toLong, i))
  }.sortBy(_.id).reverse.toList

  log(schedules)
  val count = schedules.length

  var time: Long = -schedules.head.pos
  var step = schedules.head.id
  var remaining = schedules.tail

  while (remaining.nonEmpty) {
    val current = remaining.head
    if ((time + current.pos) % current.id == 0) {
      remaining = remaining.tail
      step = step * current.id
      log(s"New step is $step starting at $time")
    } else {
      time = time + step
    }
  }

  println(time)


  case class Bus(id: Long, pos: Int)


}
