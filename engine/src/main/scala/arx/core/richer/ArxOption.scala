package arx.core.richer

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 4/12/15
 * Time: 10:23 AM
 */

import arx.Prelude._
import scalaxy.loops._

class ArxOption[T](val opt : Option[T]) extends AnyVal {
	def orPossibly (other : => Option[T]) = {
		if (opt.isEmpty) {
			other
		} else {
			opt
		}
	}

	def ifPresent (func : (T) => Unit): Unit = {
		if (opt.isDefined) {
			func(opt.get)
		}
	}
}
