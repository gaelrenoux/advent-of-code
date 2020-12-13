package y2020.d13

import scala.collection.mutable
import scala.util.Try
import y2020.log

// scalastyle:off
object MainD13Star1 extends App {

  val text = Input.real
  val lines = text.split("\n").map(_.trim).filter(_.nonEmpty)
  val time = lines.head.toLong
  log(time)
  val schedules = lines(1).split(",").flatMap(i => Try(i.toInt).toOption)
  log(schedules.toList)

  val pile = mutable.PriorityQueue(schedules.map(i => Next(i, i)): _*)(Next.NextOrdering.reverse)

  var head = pile.dequeue()
  while (head.time < time) {
    pile.enqueue(head.incr)
    head = pile.dequeue()
  }

  println(head)
  val waiting = head.time - time
  println(waiting * head.id)

}

case class Next(id: Int, time: Int) {
  def incr = copy(time = time + id)
}
object Next {
  implicit object NextOrdering extends Ordering[Next] {
    private val intOrdering = implicitly[Ordering[Int]]
    override def compare(x: Next, y: Next): Int = intOrdering.compare(x.time, y.time)
  }
}
