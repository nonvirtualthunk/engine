package arx.engine.data

import arx.core.introspection.ReflectionAssistant

import scala.collection.mutable


/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 8:46 AM
 */

trait THasInternalAuxData[U <: TAuxData] extends THasAuxData[U] {
	var _auxData = new mutable.HashMap[Class[_], U]()

	def auxData[T <: U](man : Class[T]): T = {
		if (_auxData.contains(man)) {
			_auxData(man).asInstanceOf[T]
		} else {
			val n = ReflectionAssistant.instantiate(man)
			addAuxData(n)
			n
		}
	}

	def auxDataOrElse[T <: U : Manifest](orElse: T): T = {
		val man = manifest[T].runtimeClass

		_auxData.getOrElse(man, orElse).asInstanceOf[T]
	}

	def removeAuxData[T <: U](clazz: Class[T]) { _auxData -= clazz }

	protected def storeAuxData(d: U): Unit = {
		_auxData(d.getClass) = d
	}

	def auxDataOpt[T <: U : Manifest]: Option[T] = {
		val clazz = manifest[T].runtimeClass
		_auxData.get(clazz).asInstanceOf[Option[T]]
	}

	def hasAuxData[T <: U : Manifest] = _auxData.contains(manifest[T].runtimeClass)

	def allAuxData = _auxData.values.toList
}
