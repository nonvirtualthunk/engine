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
class IntRange ( val lower : Int , val upper : Int ) extends Serializable {
	def contains ( f : Int ) = lower <= f && upper >= f
	def length = upper - lower + 1 //+1 because it is inclusive on both ends

	override def toString () = lower + " -> " + upper
}