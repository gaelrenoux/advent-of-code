package y2020.d13

import scala.util.Random

/*
 * Reference: https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
 */
object ExtEuclidian {

  def apply(a: Int, b: Int) = {

    // Represents one iteration in the algorithm
    case class Iteration(
        r: Int, rPrime: Int,
        x: Int, xPrime: Int,
        y: Int, yPrime: Int
    )

    // Recursively execute the algorithm and finally return (x, y)
    def EEA(i: Iteration): (Int, Int) =
      if (i.rPrime != 0) {
        val d = Math.floor(i.r / i.rPrime).toInt

        val next =
          Iteration(
            i.rPrime, i.r - d * i.rPrime,
            i.xPrime, i.x - d * i.xPrime,
            i.yPrime, i.y - d * i.yPrime
          )

        EEA(next)
      } else
        (i.x, i.y) //> EEA: (i: ExtendedEuclideanAlgorithm.Iteration)(Int, Int)

    // Initial values for a, b, x, x', y, y'
    val init = Iteration(a, b, 1, 0, 0, 1) //> init  : ExtendedEuclideanAlgorithm.Iteration = Iteration(28,92,1,0,0,1)

    // Execute the algorithm
    val (x, y) = EEA(init) //> x  : Int = 10
    //| y  : Int = -3

    (x, y)
  }
}
