package arx.core.math

import arx.Prelude
import arx.core.traits.TArxNumeric
import arx.core.units.UnitOfTime
import arx.core.vec.coordinates.{CartVec, CartVec3}
import arx.core.vec.{ReadVec2f, ReadVec2i, Vec2i}
import arx.engine.data.Reduceable
import arx.graphics.helpers.HSBA

import scala.language.implicitConversions

trait Interpolation[T] {
	def interpolate(pcnt : Float) : T

	def interpolate(startTime : UnitOfTime, duration : UnitOfTime, curTime : UnitOfTime) : T =
		interpolate((curTime - startTime).inSeconds / duration.inSeconds)

	def sin010 : Interpolation[T] = {
		val outer = this
		new Interpolation[T] {
			override def interpolate(pcnt : Float) : T = {
				outer.interpolate(Prelude.sinf(pcnt * math.Pi))
			}
		}
	}

	def sin01 : Interpolation[T] = {
		val outer = this
		new Interpolation[T] {
			override def interpolate(pcnt : Float) : T = {
				outer.interpolate(Prelude.sinf(pcnt * math.Pi * 0.5f))
			}
		}
	}

	def sin10 : Interpolation[T] = {
		val outer = this
		new Interpolation[T] {
			override def interpolate(pcnt : Float) : T = {
				outer.interpolate(Prelude.sinf(math.Pi * 0.5f + pcnt * math.Pi * 0.5f))
			}
		}
	}

	def map[U](f : T => U) : Interpolation[U] = {
		val outer = this
		new Interpolation[U] {
			override def interpolate(pcnt: Float): U = {
				f(outer.interpolate(pcnt))
			}
		}
	}
}

//class LinearInterpolation[T <: { def -(a : T) : T; def * (f : Float) : T; def +(a : T) : T }](a : T, b : T) extends Interpolation[T] {
//	override def interpolate(pcnt: Float): T = a + (b - a) * pcnt
//}

object Interpolation {
	def constant[T](v : T) : Interpolation[T] = new Interpolation[T] {
		override def interpolate(pcnt: Float): T = v
	}

	def between(a : Int, b : Int) : Interpolation[Int] = new Interpolation[Int] {
		override def interpolate(pcnt: Float): Int = (a.toFloat + (b-a) * pcnt).round
	}

	def between(a : Float, b : Float) : Interpolation[Float] = new Interpolation[Float] {
		override def interpolate(pcnt: Float): Float = a + (b-a) * pcnt
	}

	def between(a : ReadVec2i, b : ReadVec2i) : Interpolation[ReadVec2i] = new Interpolation[ReadVec2i] {
		override def interpolate(pcnt: Float): ReadVec2i = (a + (b-a) * pcnt).round
	}

	def between(a : ReadVec2f, b : ReadVec2f) : Interpolation[ReadVec2f] = new Interpolation[ReadVec2f] {
		override def interpolate(pcnt: Float): ReadVec2f = a + (b-a) * pcnt
	}

	def between(a : CartVec3, b : CartVec3) : Interpolation[CartVec3] = new Interpolation[CartVec3] {
		override def interpolate(pcnt: Float): CartVec3 = a + (b-a) * pcnt
	}

	def between(a : HSBA, b : HSBA) : Interpolation[HSBA] = new Interpolation[HSBA] {
		override def interpolate(pcnt: Float): HSBA = HSBA(a + (b-a) * pcnt)
	}

	def between[T <: TArxNumeric[T]](a : T , b : T) : Interpolation[T] = new Interpolation[T] {
		override def interpolate(pcnt: Float): T = a + (b-a) * pcnt
	}

	def betweenI(a : Reduceable[Int], b : Reduceable[Int]) : Interpolation[Reduceable[Int]] = new Interpolation[Reduceable[Int]] {
		override def interpolate(pcnt: Float): Reduceable[Int] = new Reduceable[Int]((a.baseValue + (b.baseValue - a.baseValue) * pcnt).round, (a.reducedBy + (b.reducedBy - a.reducedBy) * pcnt).round)
	}

//	def between[T : Numeric](a : Reduceable[T], b : Reduceable[T]) : Interpolation[Reduceable[T]] = new Interpolation[Reduceable[T]] {
//		override def interpolate(pcnt: Float): Reduceable[T] = {
//			val NUM = implicitly[Numeric[T]]
//			new Reduceable[T]((NUM.plus(a.baseValue, NUM.toFloat(NUM.minus(b.baseValue, a.baseValue)) * pcnt)))
//		}
//	}


	def frames[T](frames : Seq[T]) : Interpolation[T] = new Interpolation[T] {
		override def interpolate(pcnt: Float): T = {
			val index = (frames.size * pcnt).toInt.min(frames.size-1)
			frames(index)
		}
	}

	implicit def toConstantInterpolation[T](v : T) : Interpolation[T] = constant(v)

	implicit def functionInterpolation[T](f : Float => T) : Interpolation[T] = new Interpolation[T] {
		override def interpolate(pcnt: Float): T = f(pcnt)
	}
}