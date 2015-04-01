/**
 * Created by kiselev on 22.11.14.
 */

class Matrix(val values: Array[Array[Double]]) {
  assert(values(0).length != 0, println("Matrix should not be empty"))

  override def clone() = values.map(_.clone()).clone()

  lazy val dim = values.length
  assert((0 until dim).forall(values.length == values(_).length), println("Matrix should be squared"))

  lazy val trace = (for (i <- 0 until dim) yield this.values(i)(i)).sum

  def +(that: Matrix) = {
    assert(this.dim == that.dim, "Dimensions should be equal")
    new Matrix(
      (for (i <- 0 until dim) yield
        (for (j <- 0 until dim) yield
          that.values(i)(j) + this.values(i)(j)).toArray).toArray)
  }

  lazy val t = new Matrix(// matrix transposition
    (for (i <- 0 until dim) yield
      (for (j <- 0 until dim) yield
        values(j)(i)).toArray).toArray
  )

  def *(that: Matrix) = {
    def dotProduct(vec1: Array[Double], vec2: Array[Double]): Double = {
      (for (i <- 0 until dim; value = vec1(i) * vec2(i)) yield value).sum
    }
    new Matrix(
      (for (i <- 0 until dim) yield
        (for (j <- 0 until dim) yield
          dotProduct(this.values(i), that.t.values(j))).toArray).toArray)
  }

  def *(num: Double) = new Matrix(
    values.map(_.map(_ * num))
  )

  def /(num: Double) = this * (1 / num)

  def -(that: Matrix) = this + that * (-1)

  lazy val norm_inf: Double = (0 until dim).map(values(_).toList.reduce(_ + _.abs)).max

  lazy val norm_1: Double = this.t.norm_inf

  lazy val LU: (Matrix, Matrix) = {
    val a = values.map(_.clone()).clone()
    val b = Array.ofDim[Double](dim, dim)
    val c = Array.ofDim[Double](dim, dim)
    for (k <- 0 until dim; m <- k until dim) {
      b(m)(k) = a(m)(k) - (for (j <- 0 until k) yield b(m)(j) * c(j)(k)).sum
      c(k)(m) = (a(k)(m) - (for (j <- 0 until k) yield b(k)(j) * c(j)(m)).sum) / b(k)(k)
    }
    (new Matrix(b), new Matrix(c))


  lazy val symmetrical: Boolean = ((0 until dim) zip (0 until dim))
    .forall({ case (i, j) => values(i)(j) == values(j)(i)})

  lazy val max_abs = values.map(_.map(_.abs).max).max

  lazy val max_at =
    (for (i <- 0 until dim; j <- 0 until dim if values(i)(j).abs == max_abs)
    yield (i, j)).head

  lazy val row_dominant: Boolean =
    (0 until dim).forall(i => values(i)(i) >
      (for (j <- 0 until dim if j != i; a = values(i)(j).abs) yield a).sum)

  lazy val column_dominant: Boolean = this.t.row_dominant

  def *(vec: MVector) = {
    assert(vec.dim == this.dim, "Dimensions of matrix and vector should be equal")
    new MVector(
      (for (i <- 0 until dim) yield new MVector(this.values(i)) * vec).toArray
    )
  }

  def apply(i: Int, j: Int) = this.values(i)(j)

  def apply(i: Int) = new MVector(this.values(i))

  override def toString: String = values.map(_.mkString("[", ", ", "]")).mkString("\n")

  def update(i: Int, vec: MVector) = {
    val new_values = clone()
    new_values(i) = vec.values
    new Matrix(new_values)
  }

  def update(i: Int, j: Int, num: Double) = {
    val new_values = clone()
    new_values(i)(j) = num
    new Matrix(new_values)
  }

  def swapRows(i: Int, j: Int) = {
    val new_values = clone()
    new_values(i) = values(j)
    new_values(j) = values(i)
    new Matrix(new_values)
  }

  def swapColumns(i: Int, j: Int) = this.t.swapRows(i, j).t

  def copy = new Matrix(clone())

  lazy val JacobiMatrix = {
    val L_values = Array.ofDim[Double](dim, dim)
    for (i <- 0 until dim; j <- i + 1 until dim) L_values(i)(j) = values(i)(j)
    val L = new Matrix(L_values)

    val D_neg_one_values = Array.ofDim[Double](dim, dim)
    for (i <- 0 until dim) D_neg_one_values(i)(i) = 1 / values(i)(i)
    val D_neg_one = new Matrix(D_neg_one_values)

    val R_values = Array.ofDim[Double](dim, dim)
    for (j <- 0 until dim; i <- j + 1 until dim) L_values(i)(j) = values(i)(j)
    val R = new Matrix(R_values)

    D_neg_one * (L + R) * (-1)
  }

  lazy val JacobiDiagonal = {
    val D_neg_one_values = Array.ofDim[Double](dim, dim)
    for (i <- 0 until dim) D_neg_one_values(i)(i) = 1 / values(i)(i)
    new Matrix(D_neg_one_values)
  }
  import scala.language.implicitConversions
  implicit def doubleToMatrix(num: Double): Matrix = new Matrix(Array(Array(num)))
}