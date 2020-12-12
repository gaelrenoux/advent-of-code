package y2020.d11

sealed abstract class Direction(val dx: Int, val dy: Int)
object North extends Direction(0, -1)
object South extends Direction(0, 1)
object West extends Direction(-1, 0)
object East extends Direction(1, 0)
object NorthWest extends Direction(-1, -1)
object NorthEast extends Direction(1, -1)
object SouthWest extends Direction(-1, 1)
object SouthEast extends Direction(1, 1)

object Direction {
  val All = Seq(North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest)
}
