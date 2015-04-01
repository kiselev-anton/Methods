/**
 * Created by kiselev on 23.11.14.
 */

class MVector(val values: Array[Double]) {
  val dim = values.length
  assert(dim != 0, println("Vector length should be > 0"))

  def +(that: MVector): MVector = {
    new MVector((this.values.toList zip that.values.toList).map({ case (x, y) => x + y}).toArray)
  }

  def -(that: MVector) = this + that * (-1)

  def *(that: MVector): Double = {
    assert(this.dim == that.dim, println("Vector length in scalar product should be equal"))
    (this.values.toList zip that.values.toList).map({ case (x, y) => x * y}).sum
  }

  def *(num: Double): MVector = new MVector(
    values.map(_ * num)
  )

  def /(num: Double): MVector = this * (1 / num)

  lazy val norm_inf: Double = values.map(math.abs(_)).max
  lazy val norm_2: Double = math.sqrt(this * this)
  lazy val norm_1: Double = values.toList.map(math.abs(_)).sum

  def norm(s: String) = s match {
    case "1" => norm_1
    case "2" => norm_2
    case "inf" => norm_inf
  }

  override def toString = this.values.mkString("[", ", ", "]")

  def apply(n: Int) = this.values(n)

  def update(i: Int, num: Double) = {
    val new_values = new Array[Double](dim)
    Array.copy(values, 0, new_values, 0, dim)
    new_values(i) = num
    new MVector(new_values)
  }

  def swap(i: Int, j: Int) = {
    val new_values = new Array[Double](dim)
    Array.copy(values, 0, new_values, 0, dim)
    new_values(i) = values(j)
    new_values(j) = values(i)
    new MVector(new_values)
  }

  def copy = new MVector(values)

  def ==(that: MVector) = this.values == that.values
  import scala.language.implicitConversions
  implicit def doubleToVector(num: Double): MVector = new MVector(Array(num))
}
