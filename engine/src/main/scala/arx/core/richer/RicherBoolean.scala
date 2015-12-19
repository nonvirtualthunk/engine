package arx.core.richer

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/26/14
 * Time: 1:55 PM
 */

import arx.Prelude._
import scalaxy.loops._

class RicherBoolean (val bool : Boolean) extends AnyVal {
	def ?[X](t: => X) = new {
		def |(f: => X) = if(bool) t else f
	}
}
