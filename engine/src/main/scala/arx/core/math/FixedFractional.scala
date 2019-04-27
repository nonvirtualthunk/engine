package arx.core.math

import scala.language.implicitConversions

abstract class FixedFractional[T <: FixedFractional[T]](protected[math] val raw : Int) {
	def radix : Int
	def create(raw : Int) : T

	def + (other : FixedFractional[_]) : T = create((this.raw * other.radix + other.raw * this.radix) / other.radix)
	def - (other : T) : T = create(this.raw - other.raw)
	def * (other : T) : T = create((this.raw * other.raw) / radix)
	def / (other : T) : T = create((this.raw / other.raw) * radix)
	def unary_- : T = create(-this.raw)

	def + (other : Int) : T = create(this.raw + (other * radix))
	def - (other : Int) : T = create(this.raw - (other * radix))
	def * (other : Int) : T = create(this.raw * (other * radix))
	def / (other : Int) : T = create(this.raw / (other * radix))

	def asInt : Int = raw / radix
	def asFloat : Float = raw.toFloat / radix.toFloat
	def asDouble : Double = raw.toDouble / radix.toDouble
	def asWholeAndFraction : (Int,Int) = (raw / radix, raw % radix)

	def < (other : T) : Boolean = this.raw < other.raw
	def > (other : T) : Boolean = this.raw > other.raw
	def >= (other : T) : Boolean = this.raw >= other.raw
	def <= (other : T) : Boolean = this.raw <= other.raw

	def compareTo(other : T) : Int = this.raw.compareTo(other.raw)


	override def equals(obj: Any): Boolean = {
		obj match {
			case ff : FixedFractional[_] if ff.radix == this.radix => ff.raw == this.raw
			case _ => false
		}
	}

	def toSignedString : String = {
		if (raw >= 0) {
			s"+$this"
		} else {
			s"-$this"
		}
	}

	override def toString: String = {
		val (whole,fraction) = asWholeAndFraction
		if (fraction != 0) {
			s"$whole $fraction/$radix"
		} else {
			s"$whole"
		}
	}
}

class FractionalNumeric[T <: FixedFractional[T]](override val zero : T) extends Numeric[T] {
	override def plus(x: T, y: T): T = x + y
	override def minus(x: T, y: T): T = x - y
	override def times(x: T, y: T): T = x * y
	override def negate(x: T): T = -x
	override def fromInt(x: Int): T = zero.create(x * zero.radix)
	override def toInt(x: T): Int = x.asInt
	override def toLong(x: T): Long = x.asInt
	override def toFloat(x: T): Float = x.asFloat
	override def toDouble(x: T): Double = x.asDouble
	override def compare(x: T, y: T): Int = x.raw.compareTo(y.raw)
}


class Sext private (sexts : Int) extends FixedFractional[Sext](sexts) {
	override def radix: Int = 6
	override def create(raw: Int): Sext = new Sext(raw)
}
object Sext {
	implicit def fromInt(i : Int) : Sext = ofInt(i)
	def apply(i : Int) = new Sext(i * 6)
	def ofInt(i : Int) = new Sext(i * 6)
	def ofSexts(s : Int) = new Sext(s)
	def ofWholeAndParts(w : Int, p : Int) = new Sext(w * 6 + p)

	implicit val numeric : Numeric[Sext] = new FractionalNumeric[Sext](new Sext(0))
}