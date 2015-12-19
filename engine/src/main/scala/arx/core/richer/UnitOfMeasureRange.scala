package arx.core.richer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 10:17 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.units.UnitOfMeasure

@SerialVersionUID(1L)
class UnitOfMeasureRange[T <: UnitOfMeasure[T]] ( val lower : T , val upper : T ) extends Serializable {
	def contains ( f : T ) = lower <= f && upper >= f
}