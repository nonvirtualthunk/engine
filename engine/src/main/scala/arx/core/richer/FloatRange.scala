package arx.core.richer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 10:16 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto

@SerialVersionUID(1L)
class FloatRange ( val lower : Float , val upper : Float ) extends Serializable {
	def contains ( f : Float ) = lower <= f && upper >= f
	def length = upper - lower

	override def toString: String = s"Range($lower,$upper)"
}