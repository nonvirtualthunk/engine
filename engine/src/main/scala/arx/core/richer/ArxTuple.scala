package arx.core.richer

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 12:20 PM
 */

import arx.Prelude._
import scalaxy.loops._

class ArxTuple[A,B] (val intern : (A,B)) extends AnyVal {
	def left = intern._1
	def right = intern._2
}
