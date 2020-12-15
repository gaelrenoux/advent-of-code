package y2020.d15

// scalastyle:off
object MainD15Star1 extends App {

  val init = Input.test.reverse
  val initCount = init.size

  val iterationNum = 2020
  // val iterationNum = 30000000

  var list: List[Int] = init
  var existing: Set[Int] = Set(init: _*)

  for {
    i <- initCount until iterationNum
  } {
    // if (i % 10000 == 0) println(s"Iteration $i")
    val (head :: tail) = list
    val last = if (existing.contains(head)) 1 + tail.indexOf(head) else 0
    list = last :: list
    existing = existing + last
  }

  //println(list.reverse)
  println(list.head)

}
