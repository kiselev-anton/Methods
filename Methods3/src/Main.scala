/**
 * Created by kiselev on 07.12.14.
 */

object Main extends App {

  val A = new Matrix(Array(Array(1.64,-0.06,0.2),Array(-0.1,0.86,0.28),Array(-0.16,-0.34,-0.5)))
  val b = new MVector(Array(2.12,2.46,-2.34))

  val x = new MVector(Array(1,2,3))

  println("A = "); println(A)
  println("b = " + b); println()

  println("x = " + x + " -> this i  the exact solution")
  println("Ax=" + A * x + " = " + b + " = b")

  val gaussMethod = new GaussMethod(A,b)
  val gaussResult = gaussMethod.solve
  println(); println("Results for Gauss Method:")
  println("x = " + gaussResult)
  println("Residual, using norm 1 = " + (x - gaussResult).norm("1"))
  println("Residual, using norm 2 = " + (x - gaussResult).norm("2"))
  println("Residual, using norm ∞ = " + (x - gaussResult).norm("inf"))

  val improvedGaussMethod = new GaussMethodImproved(A,b)
  val improvedGaussResult = improvedGaussMethod.solve
  println(); println("Results for Improved Gauss Method:")
  println("x = " + improvedGaussResult)
  println("Residual, using norm 1 = " + (x - improvedGaussResult).norm("1"))
  println("Residual, using norm 2 = " + (x - improvedGaussResult).norm("2"))
  println("Residual, using norm ∞ = " + (x - improvedGaussResult).norm("inf"))

  println()
  println("B = "); println(A.JacobiMatrix)
  val jacobiMethod = new JacobiMethod(A.JacobiMatrix, A.JacobiDiagonal * b, new MVector(Array(0,0,0)), 0.00005)
  val jacobiMethodResult = jacobiMethod.solve
  println(); println("Results for Jacobi Method:")
  println("x = " + jacobiMethodResult)
  println("Residual, using norm 1 = " + (x - jacobiMethodResult).norm("1"))
  println("Residual, using norm 2 = " + (x - jacobiMethodResult).norm("2"))
  println("Residual, using norm ∞ = " + (x - jacobiMethodResult).norm("inf"))

  val seidelMethod = new SeidelMethod(A.JacobiMatrix, A.JacobiDiagonal * b, new MVector(Array(0,0,0)), 0.00005)
  val seidelMethodResult = seidelMethod.solve
  println(); println("Results for Jacobi Method:")
  println("x = " + seidelMethodResult)
  println("Residual, using norm 1 = " + (x - seidelMethodResult).norm("1"))
  println("Residual, using norm 2 = " + (x - seidelMethodResult).norm("2"))
  println("Residual, using norm ∞ = " + (x - seidelMethodResult).norm("inf"))


}
