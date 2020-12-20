package y2020.d17

import scala.collection.mutable

// scalastyle:off
object MainStar1 extends App {

  type Cube = Array[Array[Array[Boolean]]]

  val CyclesCount = 6

  val input = Input.real.split("\n").map(_.trim).filter(_.nonEmpty)
  val inputSize = math.max(input.length, input.head.length)

  val startingIndex = CyclesCount
  val maxSize = startingIndex + inputSize + CyclesCount

  val worldGreen: Cube = Array.fill(maxSize, maxSize, maxSize)(false)
  val worldBlue: Cube = Array.fill(maxSize, maxSize, maxSize)(false)


  var current = worldGreen
  var next = worldBlue

  input.zipWithIndex.foreach { case (line, y) =>
    line.zipWithIndex.foreach { case (point, x) =>
      current(startingIndex + x)(startingIndex + y)(startingIndex) = (point == '#')
    }
  }

  def blueGreenSwitch(): Unit = {
    val old = current
    current = next
    next = old
  }

  private final val neighboursCache = mutable.Map[(Int, Int, Int), Array[(Int, Int, Int)]]()

  @inline private final def neighbours(x: Int, y: Int, z: Int): Array[(Int, Int, Int)] =
    neighboursCache.getOrElseUpdate((x, y, z), calculateNeighbours(x, y, z))

  @inline private final def calculateNeighbours(x: Int, y: Int, z: Int) =
    Array(
      (x - 1, y - 1, z - 1), (x, y - 1, z - 1), (x + 1, y - 1, z - 1),
      (x - 1, y, z - 1), (x, y, z - 1), (x + 1, y, z - 1),
      (x - 1, y + 1, z - 1), (x, y + 1, z - 1), (x + 1, y + 1, z - 1),
      (x - 1, y - 1, z), (x, y - 1, z), (x + 1, y - 1, z),
      (x - 1, y, z), (x + 1, y, z),
      (x - 1, y + 1, z), (x, y + 1, z), (x + 1, y + 1, z),
      (x - 1, y - 1, z + 1), (x, y - 1, z + 1), (x + 1, y - 1, z + 1),
      (x - 1, y, z + 1), (x, y, z + 1), (x + 1, y, z + 1),
      (x - 1, y + 1, z + 1), (x, y + 1, z + 1), (x + 1, y + 1, z + 1)
    ).filter {
      case (x, y, z) => x >= 0 && y >= 0 && z >= 0 && x < maxSize && y < maxSize && z < maxSize
    }

  for (cycle <- 0 until CyclesCount) {
    for {
      x <- 0 until maxSize
      y <- 0 until maxSize
      z <- 0 until maxSize
    } {
      val ns = neighbours(x, y, z)
      val isActive = current(x)(y)(z)
      val activeNs = ns.view.map { case (i, j, k) => current(i)(j)(k) }.count(identity)
      if (isActive && (activeNs == 2 || activeNs == 3)) {
        next(x)(y)(z) = true
      } else if (!isActive && activeNs == 3) {
        next(x)(y)(z) = true
      } else {
        next(x)(y)(z) = false
      }
    }
    //printCube(next)
    blueGreenSwitch()
  }

  def printCube(c: Cube) = {
    val slices = c.map(_.transpose).transpose.map(_.transpose)
    println("\nCube:")
    slices.zipWithIndex
      .dropWhile { case (slice, _) => slice.flatten.forall(!_) }
      .takeWhile { case (slice, _) => slice.flatten.exists(identity) }
      .foreach { case (slice, ix) =>
        println(s"  Slice $ix")
        slice.foreach { line =>
          println(line.map {
            case true => '#'
            case false => '.'
          }.mkString(""))
        }
        println("---------------------------")
      }
  }

  val active = current.flatten.flatten.count(identity)
  println(active)

}
