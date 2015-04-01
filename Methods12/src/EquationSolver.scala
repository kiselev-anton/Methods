abstract class EquationSolver {
	def solve: Double
}

class BisectionMethod(function: Double => Double, precision: Double,
	var startPoint: Double, var endPoint: Double) extends EquationSolver {

	var iterations = 0

	def solve = {
		var length = endPoint - startPoint
		var middlePoint: Double = 0
		while (length > precision) {
			middlePoint = (endPoint + startPoint) / 2
			if (function(startPoint) * function(middlePoint) <= 0)
				endPoint = middlePoint
			else
				startPoint = middlePoint
			length = endPoint - startPoint
		iterations += 1
		}
		middlePoint
	}
}

class SecantMethod(f: Double => Double, precision: Double,
	startPoint: Double, endPoint: Double,
	 var firstValue: Double, var secondValue: Double) extends EquationSolver {

	var iterations = 0

	private def next = secondValue - f(secondValue) * (secondValue - firstValue) / (f(secondValue) - f(firstValue))

	def solve = {
		var thirdValue = next
		while (math.abs(thirdValue - secondValue) > precision) {
			firstValue = secondValue
			secondValue = thirdValue
			thirdValue = next
			iterations += 1
		}
		thirdValue
	}
}

class FixedPointIterationMethod(phi: Double => Double, precision: Double,
	var value: Double) extends EquationSolver {

	var iterations = 0
	
	def next = phi(value)

	def solve = {
		var nextValue = next
		do {
			value = nextValue
			nextValue = next
			iterations += 1
		} while (math.abs(value - nextValue) > precision)
		nextValue
	}
}

class TaskSolver {

	def main(args: Array[String]): Unit = {
		val f: Double => Double = {x => math.tan(x) - 1 + 0.4 * x}

		val firstMethod = new BisectionMethod(f, 0.000005, 0, 1)
		println("Bisection method results: ")
		println("root = " + firstMethod.solve + ", iterations = " + firstMethod.iterations)

		val secondMethod = new SecantMethod(f, 0.000005, math.Pi / 6, math.Pi / 4, 0, 1) // 0.55, 0.77
		println("Secant method results: ")
		println("root = " + secondMethod.solve + ", iterations = " + secondMethod.iterations)

		val phi: Double => Double = {x => math.atan(1-0.4*x)}
		val thirdMethod = new FixedPointIterationMethod(phi, 0.000005, 0.5)
		println("Fixed point interation method results: ")
		println("root = " + thirdMethod.solve + ", iterations = " + thirdMethod.iterations)
	}
	
}