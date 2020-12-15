package y2020.d15

import scala.collection.mutable
import scala.util.Try
import y2020.log

// scalastyle:off
object MainD15Star2 extends App {

  val init = Input.real
  val initCount = init.size

  // val iterationNum = 2020
  val iterationNum = 30000000

  val past = mutable.Map[Int, Int](init.take(initCount - 1).zipWithIndex: _*)
  var last: Int = init.last

  init.zipWithIndex.foreach { case (a, b) =>
    println(s"$b: $a")
  }

  for {
    n <- initCount until iterationNum
  } {
    val lastApparition = past.get(last)
    val lastAge = lastApparition.fold(0) { ix => n - 1 - ix }
    past.put(last, n - 1)
    last = lastAge
    if (n % 1000000 == 0) println(s"$n: $lastAge")
  }

  //println(list.reverse)
  println(last)

}
