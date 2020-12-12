package y2020.d12

sealed abstract class Instruction(f: State => State) {
  def apply(state: State): State = f(state)
}


object Instruction {

  case class N(i: Int) extends Instruction(_.setWaypoint(_.north(i)))
  case class S(i: Int) extends Instruction(_.setWaypoint(_.south(i)))
  case class E(i: Int) extends Instruction(_.setWaypoint(_.east(i)))
  case class W(i: Int) extends Instruction(_.setWaypoint(_.west(i)))
  case class R(a: Int) extends Instruction(_.setWaypoint(_.right(a)))
  case class F(i: Int) extends Instruction(s => s.setShip(_.forward(s.waypoint, i)))

  def from(str: String): Instruction = {
    val (inst, num) = (str.head, str.drop(1).toInt)
    inst match {
      case 'N' => N(num)
      case 'S' => S(num)
      case 'E' => E(num)
      case 'W' => W(num)
      case 'L' => R(4 - (num / 90))
      case 'R' => R(num / 90)
      case 'F' => F(num)
    }
  }
}
