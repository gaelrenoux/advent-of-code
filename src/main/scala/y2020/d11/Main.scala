package y2020.d11

import scala.annotation.tailrec
import scala.util._
import y2020.log

// scalastyle:off
object Main extends App {

  val text = Input.real
  val lines = text.split("\n").map(_.trim).filter(_.nonEmpty).map(_.toArray)
  val height = lines.length
  val width = lines.head.length
  val maxX = width - 1
  val maxY = height - 1

  @inline def count(x: Int, y: Int): Int = if (lines(y)(x) == '#') 1 else 0

  def countNeighbours(x: Int, y: Int): Int = {
    if (x == 0) {
      if (y == 0) {
        count(x, y + 1) + count(x + 1, y) + count(x + 1, y + 1)
      } else if (y == maxY) {
        count(x, y - 1) + count(x + 1, y - 1) + count(x + 1, y)
      } else {
        count(x, y - 1) + count(x, y + 1) + count(x + 1, y - 1) + count(x + 1, y) + count(x + 1, y + 1)
      }
    } else if (x == maxX) {
      if (y == 0) {
        count(x - 1, y) + count(x - 1, y + 1) + count(x, y + 1)
      } else if (y == maxY) {
        count(x - 1, y - 1) + count(x - 1, y) + count(x, y - 1)
      } else {
        count(x - 1, y - 1) + count(x - 1, y) + count(x - 1, y + 1) + count(x, y - 1) + count(x, y + 1)
      }
    } else {
      if (y == 0) {
        count(x - 1, y) + count(x - 1, y + 1) + count(x, y + 1) + count(x + 1, y) + count(x + 1, y + 1)
      } else if (y == maxY) {
        count(x - 1, y - 1) + count(x - 1, y) + count(x, y - 1) + count(x + 1, y - 1) + count(x + 1, y)
      } else {
        count(x - 1, y - 1) + count(x - 1, y) + count(x - 1, y + 1) + count(x, y - 1) + count(x, y + 1) + count(x + 1, y - 1) +
          count(x + 1, y) + count(x + 1, y + 1)
      }
    }
  }

  @tailrec
  def visible(x: Int, y: Int, d: Direction): Int = {
    val i = x + d.dx
    val j = y + d.dy
    // log(s"Checking $i $j")
    if (i < 0 || i > maxX || j < 0 || j > maxY) 0
    else {
      val s = lines(j)(i)
      if (s == 'L') 0
      else if (s == '#') 1
      else visible(i, j, d)
    }
  }

  def visible(x: Int, y: Int): Int =
    Direction.All.map(visible(x, y, _)).sum

  var flag = true
  var iteration = 1

  while (flag && iteration < 10000) {
    log(s"=================\nIteration $iteration")
    iteration = iteration + 1

    val changes = for {
      y <- 0 until height
      x <- 0 until width
    } yield {
      lazy val ns = visible(x, y)
      val c = lines(y)(x)
      //  println(s"$x $y $c $ns")
      if (c == 'L' && ns == 0) {
        Some(Left(x -> y))
      } else if (c == '#' && ns >= 5) {
        Some(Right(x -> y))
      } else None
    }

    changes.foreach {
      case None => // do nothing
      case Some(Left((x, y))) => lines(y)(x) = '#'
      case Some(Right((x, y))) => lines(y)(x) = 'L'
    }

    lines.foreach { l =>
      log(l.mkString(""))
    }

    flag = changes.exists(_.nonEmpty)
  }

  println(s"$iteration iterations")
  val r = lines.flatten.count(_ == '#')
  println(s"$r seats occupied")


}
