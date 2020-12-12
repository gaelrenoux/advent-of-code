package y2020.d12

sealed abstract class Direction(pos: Int) {
  def right(a: Int): Direction = Direction.Ordered((pos + a) % 4)
}

object Direction {

  object North extends Direction(0)

  object East extends Direction(1)

  object South extends Direction(2)

  object West extends Direction(3)

  val Ordered = Array(North, East, South, West)
}
