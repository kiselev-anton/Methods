object SeriesSolver {
	def main(args: Array[String]): Unit = {
		val π = Math.PI
		val B_2 = 1.2020569032
		var N = readLine().toInt

		var result: Double = 0
		val r = -0.6
		for (n <- 1 to N) {
			result += r * 2 / ((n - r) * (n - r))
		}
		println("S_N = " + result)

		result = 0
		val M = readLine().toInt
		for (n <- 1 to M) {
			result += (2 * r * r * (2 * n - r) / (n * n * (n - r) * (n - r)))
		}
		println("P_M = " + result)
		result += π * π * r / 3
		println("P_M + rπ^2/3 = " + result)

		result = 0
		val L = readLine().toInt
		for (n <- 1 to L) {
			result += ((6 * n - 4 * r) / (n * n * n * (n - r) * (n - r)))
		}
		result *= (r * r * r)
		println("Q_L = " + result)
		result += B_2 * 4 * r * r
		println("Q_L + B_2 = " + result)
		result += π * π * r / 3
		println("Q_L + rπ^2/3 + B_2 = " + result)

	}

	
}