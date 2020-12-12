package y2020.d12

case class Waypoint(
    dx: Int,
    dy: Int
) {
  def north(i: Int): Waypoint = copy(dy = dy + i)

  def south(i: Int): Waypoint = copy(dy = dy - i)

  def west(i: Int): Waypoint = copy(dx = dx - i)

  def east(i: Int): Waypoint = copy(dx = dx + i)

  def right: Waypoint = copy(dx = dy, dy = -dx)

  def left: Waypoint = copy(dx = -dy, dy = dx)

  def opposite: Waypoint = copy(dx = -dx, dy = -dy)

  def right(a: Int): Waypoint = (a % 4) match {
    case 0 => this
    case 1 => this.right
    case 2 => this.opposite
    case 3 => this.left
  }
}
