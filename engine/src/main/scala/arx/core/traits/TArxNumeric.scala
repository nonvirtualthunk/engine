package arx.core.traits

trait TArxNumeric[T] {
	def +(other : T) : T
	def -(other : T) : T
	def *(other : Float) : T
	def zero : T
}
