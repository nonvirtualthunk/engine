package arx.core.richer

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 10:27 AM
 */

import arx.Prelude._
import scalaxy.loops._

class ArxAnyRef[T <: AnyRef](protected val intern : T) extends AnyVal {

	def ifType[U <: T : Manifest](func : (U) => Unit): Unit = {
		if (manifest[U].runtimeClass.isAssignableFrom(intern.getClass)) {
			func(intern.asInstanceOf[U])
		}
	}

	def ifType[U <: T : Manifest](func : => Unit): Unit = {
		if (manifest[U].runtimeClass.isAssignableFrom(intern.getClass)) {
			func
		}
	}

	def ifNotType[U <: T : Manifest](func : => Unit): Unit = {
		if (!manifest[U].runtimeClass.isAssignableFrom(intern.getClass)) {
			func
		}
	}
}

class ArxAny[T <: Any](protected val intern : T) extends AnyVal {
	def pmatch (f : PartialFunction[T,Unit]): Unit = {
		if (f.isDefinedAt(intern)) {
			f.apply(intern)
		}
	}
}