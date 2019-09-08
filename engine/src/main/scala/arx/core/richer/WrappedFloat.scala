package arx.core.richer

import arx.core.traits.TArxNumeric

import scala.language.implicitConversions

class WrappedFloat(val f : Float) extends TArxNumeric[WrappedFloat] {
	override def +(other: WrappedFloat): WrappedFloat = new WrappedFloat(other.f + f)
	override def -(other: WrappedFloat): WrappedFloat = new WrappedFloat(f - other.f)
	override def *(other: Float): WrappedFloat = new WrappedFloat(f * other)
	override def zero = new WrappedFloat(0.0f)
}


object WrappedFloat {
	implicit def fromRaw(f : Float) : WrappedFloat = new WrappedFloat(f)
	implicit def toRaw(wrapped : WrappedFloat) : Float = wrapped.f
}