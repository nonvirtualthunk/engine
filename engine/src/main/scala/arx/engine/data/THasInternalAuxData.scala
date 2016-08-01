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

	protected[engine] def storeAuxData(d: U): Unit = {
		_auxData(d.getClass) = d
	}

	def auxDataOpt[T <: U : Manifest]: Option[T] = {
		val clazz = manifest[T].runtimeClass
		_auxData.get(clazz).asInstanceOf[Option[T]]
	}

	def hasAuxData[T <: U : Manifest] = _auxData.contains(manifest[T].runtimeClass)

	def allAuxData = _auxData.values.toList
}


trait THasInternalListAuxData[U <: TAuxData] extends THasAuxData[U] {
	var _auxData : List[U] = Nil

	def auxData[T <: U](man : Class[T]): T = {
		_auxData.find(u => u.getClass == man) match {
			case Some(u) => u.asInstanceOf[T]
			case None =>
				val n = ReflectionAssistant.instantiate(man)
				addAuxData(n)
				n
		}
	}

	def auxDataOrElse[T <: U : Manifest](orElse: T): T = {
		_auxData.find(u => u.getClass == manifest[T].runtimeClass) match {
			case Some(u) => u.asInstanceOf[T]
			case None => orElse
		}
	}

	def removeAuxData[T <: U](clazz: Class[T]) { _auxData = _auxData.filterNot(u => u.getClass == clazz) }

	protected[engine] def storeAuxData(d: U): Unit = {
		_auxData ::= d
	}

	def auxDataOpt[T <: U : Manifest]: Option[T] = {
		val man = manifest[T].runtimeClass
		_auxData.find(u => u.getClass == man).asInstanceOf[Option[T]]
	}

	def hasAuxData[T <: U : Manifest] = _auxData.exists(u => u.getClass == manifest[T].runtimeClass)

	def allAuxData = _auxData
}
