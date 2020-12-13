package y2020.d13

import y2020.log

import scala.annotation.tailrec

// scalastyle:off
object MainD13Star2 extends App {

  val text = Input.real
  val lines = text.split("\n").map(_.trim).filter(_.nonEmpty)
  val schedulesWithoutIncrement = lines(1).split(",").zipWithIndex.flatMap {
    case ("x", _) => None
    case (txt, i) => Some(Bus(txt.toLong, i))
  }.toList
  val schedules = (Bus(0, 0) :: schedulesWithoutIncrement).sliding(2).map {
    case List(previous, current) => current.copy(incr = current.pos - previous.pos)
  }.toArray

  log(schedules.toList)
  val count = schedules.length

  @tailrec
  def isCorrect(start: Long, index: Int): Boolean =
    if (index >= count) true else {
      val current = schedules(index)
      val newTime = start + current.incr
      if (newTime % current.id == 0) isCorrect(newTime, index + 1)
      else false
    }

  def nextFactor(time: Long, index: Int, currentFactor: Long = 1): Option[Long] =
    if (index >= count) None else {
      val current = schedules(index)
      val timeForCurrent = time + current.pos
      if (timeForCurrent % current.id == 0) nextFactor(time, index + 1, currentFactor * current.id)
      else Some(currentFactor)
    }

  // println(isCorrect(1068781, 0))
  //System.exit(0)

  val max = schedules.maxBy(_.id)
  var i: Long = 217118599999959L / max.id

  println(max)

  def measureTime(iteration: Long): Long = max.id * iteration - max.pos

  var time = measureTime(i)

  while (!isCorrect(time, 0)) {
    if (i % 100000000L == 0) {
      log(s"Tried $time")
    }
    i = i + 1
    time = measureTime(i)
    if (time < 0) {
      throw new IllegalStateException(s"i was $i")
    }
  }

  println(s"Maxis ${Long.MaxValue}")
  if (time < 0) println("BAAAAAAAAAAAAAAAAAAAAD")
  println(time)

}

case class Bus(id: Long, pos: Int, incr: Int = 0)
