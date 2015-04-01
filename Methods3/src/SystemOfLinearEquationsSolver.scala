/**
 * Created by kiselev on 30.11.14.
 */

abstract class SystemOfLinearEquationsSolver {
  val A: Matrix
  val b: MVector
  assert(A.dim == b.dim, "Dimensions of matrix and vector should be equal")
  def solve: MVector
}

class GaussMethod(val A: Matrix, val b: MVector) extends SystemOfLinearEquationsSolver {
  def solve = {
    val (l, u) = A.LU
    val vec_z = new Array[Double](b.dim)
    for (j <- 0 until b.dim) {
      vec_z(j) = (b(j) - (for (i <- 0 until j) yield vec_z(i) * l(j)(i)).sum) / l(j)(j)
    }
    val z = new MVector(vec_z)
    val vec_x = new Array[Double](b.dim)
    for (j <- (0 until b.dim).reverse) {
      vec_x(j) = z(j)  - (for (i <- j+1 until b.dim) yield vec_x(i) * u(j)(i)).sum
    }
    new MVector(vec_x)
  }
}


class GaussMethodImproved(val A: Matrix, val b: MVector) extends SystemOfLinearEquationsSolver {
  private val dim = A.dim
  def solve = {
    var M = A
    var v = b

    for (k <- 0 until dim) { // forward Gauss method step
      def max_at(k: Int) = {
        var current_max = M(k,k); var pos = (k,k)
        for (i <- k until dim; j <- k until dim; value = M(i,j))
          if (math.abs(value) > math.abs(current_max)) {pos = (i,j); current_max = value}
        pos
      }
      def swapRowsColumns(k: Int) = {
        val (m,n) = max_at(k)
        M = M.swapColumns(k,n)
        M = M.swapRows(k,m)
        v = v.swap(k,m)
      }
      swapRowsColumns(k)

      v = v(k) /= M(k,k) // norming k-s row
      M = M(k) /= M(k,k)
      for (i <- k+1 until dim) { // k+1 row
        v = v(i) -= v(k) * M(i,k)
        M = M(i) = M(i) - M(k) * M(i,k)
      }
    }

    for (j <- (1 until dim).reverse) { // backward Gauss method step
      for (i <- (0 until j).reverse) {
        v = v(i) -= M(i,j) * v(j)
        M = M(i) = M(i) - M(j) * M(i,j)
      }
    }
    v
  }

}


class JacobiMethod(val A: Matrix, val b: MVector, startVector: MVector,
                   precision: Double) extends SystemOfLinearEquationsSolver {

  assert(precision > 0, "Precision should be > 0")

  var iterations = 0

  def methodApplication(vec: MVector): MVector = {
    iterations += 1
    A * vec + b
  }
  private var previousVector: MVector = startVector
  private var currentVector: MVector = methodApplication(startVector)

  def solve = {
    while ((previousVector - currentVector).norm_1 > precision) {
      previousVector = currentVector
      currentVector = methodApplication(currentVector)
    }
    currentVector
  }
}


class SeidelMethod(val A: Matrix, val b: MVector, startVector: MVector,
                   precision: Double) extends SystemOfLinearEquationsSolver {
  assert(precision > 0, "Precision should be > 0")
  var iterations = 0
  def methodApplication(vec: MVector): MVector = {
    val new_vec = new Array[Double](b.dim)
    for (i <- 0 until b.dim)
      new_vec(i) = (i until b.dim).map(j => A(i)(j) * vec(j)).sum +
                    (0 until i).map(j => A(i)(j) * new_vec(j)).sum + b(i)
    iterations += 1
    new MVector(new_vec)
  }
  private var previousVector, currentVector = startVector

  def solve = {
    currentVector = methodApplication(startVector)
    while ((previousVector - currentVector).norm_1 > precision) {
      previousVector = currentVector
      currentVector = methodApplication(currentVector)
    }
    currentVector
  }
}




