package arx.core.richer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 9:28 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.traits.TArxNumeric
import arx.core.units.UnitOfMeasure

class RicherFloat ( val f : Float ) extends AnyVal {
	def =~= ( a : Float ) : Boolean = aeq(a,0.00001f)
	def aeq ( a : Float , eps : Float ) : Boolean = { scala.math.abs(a - f) < eps }
	def +- ( a : Float ) : EitherFloat = { new EitherFloat(f + a,f - a) }
	def -+ ( a : Float ) : EitherFloat = { new EitherFloat(f - a,f + a) }
	def + (o : RicherFloat) : RicherFloat = new RicherFloat(f + o.f)

	def clamp ( minimum : Float , maximum : Float ) : Float = { scala.math.min(maximum,scala.math.max(f,minimum)) }
	def clampFloor ( minimum : Float ) : Float = scala.math.max(f,minimum)
	def clampCeil ( maximum : Float ) : Float = scala.math.min(f,maximum)
	def ceil = math.ceil(f)
	def ceili = math.ceil(f).toInt


	def *[T <: UnitOfMeasure[T]] (u : UnitOfMeasure[T]) = u * f

	def wrapped = new WrappedFloat(f)
}

class RicherDouble ( val d : Double ) extends AnyVal {
	def =~= ( a : Double ) : Boolean = aeq(a,0.00001f)
	def aeq ( a : Double , eps : Double ) : Boolean = { scala.math.abs(a - d) < eps }

	def clamp ( minimum : Double , maximum : Double ) : Double = { scala.math.min(maximum,scala.math.max(d,minimum)) }
	def clampFloor ( minimum : Double ) : Double = scala.math.max(d,minimum)
	def clampCeil ( maximum : Double ) : Double = scala.math.min(d,maximum)


	def *[T <: UnitOfMeasure[T]] (u : UnitOfMeasure[T]) = u * d.toFloat
}