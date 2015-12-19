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

class RicherInt ( val f : Int ) extends AnyVal {
	def +- ( a : Int ) : EitherInt = { new EitherInt(f + a,f - a) }
	def -+ ( a : Int ) : EitherInt = { new EitherInt(f - a,f + a) }

	def clamp ( minimum : Int , maximum : Int ) : Int = { scala.math.min(maximum,scala.math.max(f,minimum)) }
	def isBitSet ( bitFlag : Int ) = bitSet(f,bitFlag)
	def anyBitsSet ( bitMask : Int ) = (f & bitMask) != 0
}