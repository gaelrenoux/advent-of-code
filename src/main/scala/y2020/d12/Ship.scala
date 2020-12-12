package y2020.d12

case class Ship(
    x: Int,
    y: Int
) {
  def north(i: Int): Ship = copy(y = y - i)

  def south(i: Int): Ship = copy(y = y + i)

  def west(i: Int): Ship = copy(x = x - i)

  def east(i: Int): Ship = copy(x = x + i)

  def forward(w: Waypoint, i: Int): Ship =
    copy(x = x + i * w.dx, y = y + i * w.dy)
}
