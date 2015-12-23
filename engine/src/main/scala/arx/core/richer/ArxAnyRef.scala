package arx.core.richer

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 10:27 AM
 */

import arx.Prelude._
import scalaxy.loops._

class ArxAnyRef[T <: AnyRef](val intern : T) extends AnyVal {

	def ifType[U <: T : Manifest](func : (U) => Unit): Unit = {
		if (manifest[U].runtimeClass.isAssignableFrom(intern.getClass)) {
			func(intern.asInstanceOf[U])
		}
	}
}

class ArxAny[T <: Any](val intern : T) extends AnyVal {
	def pmatch (f : PartialFunction[T,Unit]): Unit = {
		if (f.isDefinedAt(intern)) {
			f.apply(intern)
		}
	}
}