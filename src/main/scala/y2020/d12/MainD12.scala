package y2020.d12

import y2020.d12.Direction._

// scalastyle:off
object MainD12 extends App {

  val text = Input.real
  val lines = text.split("\n").map(_.trim).filter(_.nonEmpty)
  val height = lines.length
  val width = lines.head.length

  val instructions = lines.map(Instruction.from)
  val init = State(
    Ship(0, 0),
    Waypoint(10, 1)
  )
  val finalState = instructions.foldLeft(init)((s, i) => i(s))

  println(finalState)
  val manDistance = math.abs(finalState.ship.x) + math.abs(finalState.ship.y)
  println(manDistance)

}
