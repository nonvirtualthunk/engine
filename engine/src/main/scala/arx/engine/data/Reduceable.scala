package arx.engine.data

class Reduceable[T : Numeric](val baseValue : T, val reducedBy : T) {
	def this(baseValueIn : T) {
		this(baseValueIn, implicitly[Numeric[T]].zero)
	}
	private def NUM = implicitly[Numeric[T]]

	def currentValue = NUM.minus(baseValue, reducedBy)
	def maxValue = baseValue
	def reduceBy(n : T, limitToZero : Boolean) = {
		var newReducedBy = NUM.plus(reducedBy, n)
		if (limitToZero) {
			newReducedBy = NUM.min(newReducedBy, baseValue)
		}
		new Reduceable(baseValue, newReducedBy)
	}
	def recoverBy(n : T, limitToZero : Boolean) = {
		var newReducedBy = NUM.minus(reducedBy, n)
		if (limitToZero) {
			newReducedBy = NUM.max(newReducedBy, NUM.zero)
		}
		new Reduceable(baseValue, newReducedBy)
	}
	def recoverToFull() = new Reduceable(baseValue)

	override def toString: String = {
		s"$currentValue ($maxValue)"
	}
}

object Reduceable {
	def apply[T : Numeric](baseValue : T) = new Reduceable(baseValue)
}