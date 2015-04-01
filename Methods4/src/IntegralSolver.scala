/**
 * Created by kiselev on 08.03.15.
 */
abstract class IntegralSolver {

  val errorOrder: Int
  val formula: Double => Double
  val a,b: Double
  def solve(steps: Int): Double
  def solve(step: Double): Double
  def rungeError(n: Int): Double =
    (solve(n / 2) - solve(n)) / (math.pow(2, errorOrder) - 1)
  def rungeError(h: Double): Double =
    (solve(h / 2) - solve(h)) / (math.pow(2, errorOrder) - 1)

}


class EulerIntegralMethod(val formula: Double => Double,
                          val formulaDerivative: Double => Double,
                          val a: Double, val b: Double) extends IntegralSolver {
  val errorOrder = 4

  def solve(steps: Int) = {
    assert(steps != 0)
    val h = (b - a) / steps
    solve(h)
  }

  def solve(h: Double) = {
    assert(h != 0)
    var result: Double = 0
    var position = a
    while (position + h <= b) {
      result += formula(position) + formula(position + h)
      position += h
    }
    result *= h / 2
    result += (h * h / 12) * (formulaDerivative(a) - formulaDerivative(b))
    result
  }

}


class MidpointRectangleMethod(val formula: Double => Double,
                              val a: Double, val b: Double) extends IntegralSolver {
  val errorOrder = 2

  def solve(steps: Int) = {
    assert(steps != 0)
    val h = (b - a) / steps
    var result: Double = 0
    var position = a
    for (i <- 1 until steps) {
      result += formula((position + position + h)/2)
      position += h
    }
    result *= h
    result
  }

  def solve(h: Double) = {
    assert(h != 0)
    var result: Double = 0
    var position = a
    while (position < (b-h)) {
      result += formula(position + h/2)
      position += h
    }
    result *= h
    result
  }
}
