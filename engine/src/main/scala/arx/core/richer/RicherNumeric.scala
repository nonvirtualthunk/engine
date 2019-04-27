package arx.core.richer

class RicherNumeric[T : Numeric](val number : T) {
	def isNegative = compare0 < 0
	def isPositive = compare0 > 0
	def compare0 = implicitly[Numeric[T]].compare(number, implicitly[Numeric[T]].zero)

	def negate = implicitly[Numeric[T]].negate(number)
}
