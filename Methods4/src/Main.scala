/**
 * Created by kiselev on 08.03.15.
 */

object Main extends App {

  val function = (x: Double) => math.sin(1 + x * x)
  val functionDerivative = (x: Double) => math.cos(1 + x*x) * 2 * x
  val a: Double = 2
  val b: Double = 3

  val steps = List(0.1, 0.05, 0.025)
  // result should be 0.186267
  val eulerMethod = new EulerIntegralMethod(function, functionDerivative, a, b)
  val midpointMethod = new MidpointRectangleMethod(function, a, b)

  for (step <- steps) {
    println("Euler method results:")
    println("h = " + step + "; I = " + eulerMethod.solve(step) +
      "; R = " + eulerMethod.rungeError(step))
    println("Midpoint method results:")
    println("h = " + step + "; I = " + midpointMethod.solve(step) +
      "; R = " + midpointMethod.rungeError(step))
  }
  println("Euler method results, extreme:")
  println("h = " + 0.0000001 + "; I = " + eulerMethod.solve(0.0000001) +
    "; R = " + eulerMethod.rungeError(0.0000001))
  println("Midpoint method results, extreme:")
  println("h = " + 0.0000001 + "; I = " + midpointMethod.solve(0.0000001) +
    "; R = " + midpointMethod.rungeError(0.0000001))


}