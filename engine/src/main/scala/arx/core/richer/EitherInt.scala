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

class EitherInt ( val a : Int , val b : Int ) {
	def + ( p : Int ) : EitherInt = new EitherInt(a + p,b + p)
	def - ( p : Int ) : EitherInt = new EitherInt(a - p,b - p)
	def * ( p : Int ) : EitherInt = new EitherInt(a * p,b * p)
}