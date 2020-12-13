package y2020.d13

import y2020.log

import scala.annotation.tailrec

// scalastyle:off
object MainD13Star2Math extends App {

  case class Bus(id: Long, pos: Int, incr: Int = 0)

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

  /*
  * Soit T le temps recherché
  * Pour chaque schedule : T = pos modulo id
  * Les id sont premiers entre eux
  */

  val ns = schedules.map(_.id)
  val bigBigN = ns.product
  val bigNs = ns.map(bigBigN / _)
  val as = schedules.map(_.pos)

  println(bigBigN)

  val (bigMs, ms) = (bigNs zip ns).map { case (bigN, n) =>
    val (bigM, m) = ExtEuclidian(bigN.toInt, n.toInt)
    println((bigM * bigM + m * n))
    (bigM, m)
  }.unzip

  println("bigMs: " + bigMs.toList)
  println("ms: " + ms.toList)

  val aSolution = (as zip bigMs zip bigNs).map {
    case ((a, bigM), bigN) => a.toLong * bigM.toLong * bigN.toLong
  }.sum

  val theSolution = aSolution % bigBigN
  println(theSolution)
  // println(isCorrect(1068781, 0))
  //System.exit(0)

}

