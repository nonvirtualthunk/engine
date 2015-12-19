package arx.core.richer

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 7/14/14
 * Time: 7:39 AM
 */

import arx.Prelude._
import arx.core.introspection.ReflectionAssistant
import scalaxy.loops._

class ArxClass[T] (val clazz : Class[T]) extends AnyVal {

	def allInterfaces : Set[Class[_]] = {
		val baseInterfaces = clazz.getInterfaces.toSet

		val fromSuper = superClass match {
			case Some(s) => s.allInterfaces
			case None => Set()
		}
		val recursive = baseInterfaces.flatMap(_.allInterfaces)

		baseInterfaces ++ fromSuper ++ recursive
	}

	def superClass = if (clazz == classOf[Object] || clazz.getSuperclass == null) {
		None
	} else {
		Some(clazz.getSuperclass)
	}

	def newArray (dim : Int) : Array[T] = ReflectionAssistant.arrayOf[T](clazz,dim)
}
