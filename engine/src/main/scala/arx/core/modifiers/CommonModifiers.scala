package arx.core.modifiers

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 11/5/12
 * Time: 4:20 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.units.{UnitOfMeasure, UnitOfSpeed}

object CommonModifiers {

	case class MultiplyByConstant ( f : Float ) extends ((Float) => Float) {
		override def apply(v1: Float) = v1 * f
	}
	case class MultiplyUOMByConstant[T <: UnitOfMeasure[T]] ( f : Float ) extends ((T) => T) {
		override def apply(v1: T) = v1 * f
	}
	case class MultiplySpeedByConstant ( f : Float ) extends ((UnitOfSpeed) => UnitOfSpeed) {
		override def apply(v1: UnitOfSpeed) = v1 * f
	}
}