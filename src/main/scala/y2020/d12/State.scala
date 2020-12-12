package y2020.d12

case class State(
    ship: Ship,
    waypoint: Waypoint
) {
  def setShip(f: Ship => Ship): State = copy(ship = f(ship))

  def setWaypoint(f: Waypoint => Waypoint): State = copy(waypoint = f(waypoint))
}
