package arx.engine.data

import arx.application.Noto
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

	def auxDataByClass[T <: U](man : Class[T]): T = {
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

	def hasAuxDataByClass[T <: U](clazz : Class[T]) = _auxData.contains(clazz)

	def allAuxData = _auxData.values.toList
}


trait THasInternalListAuxData[U <: TAuxData] extends THasAuxData[U] {
	var _auxData : List[U] = Nil

	def auxDataByClass[T <: U](man : Class[T]): T = {
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

	def hasAuxDataByClass[T <: U](clazz : Class[T]) = _auxData.exists(u => u.getClass == clazz)

	def allAuxData = _auxData
}


trait TCopyOnWriteAuxData[U <: TAuxData] extends THasInternalAuxData[U] {
	var removedClasses = Set[Class[_]]()

	def base : THasAuxData[U]

	override def auxDataByClass[T <: U](clazz: Class[T]): T = {
		if (super.hasAuxDataByClass(clazz) || !base.hasAuxDataByClass(clazz) || removedClasses(clazz)) {
			super.auxDataByClass(clazz)
		} else {
			val copied = base.auxDataByClass(clazz).copyOnWrite()
			storeAuxData(copied)
			copied
		}
	}
	override def auxDataOrElse[T <: U : Manifest](orElse: T): T = {
		if (hasAuxDataByClass(manifest[T].runtimeClass.asInstanceOf[Class[T]])) {
			auxDataByClass(manifest[T].runtimeClass.asInstanceOf[Class[T]])
		} else {
			orElse
		}
	}
	override def allAuxData: List[U] = {
		Noto.warn("Calling allAuxData on a copy-on-write hasAuxData is inadvisable")
		base.allAuxData // grab all that exist on the base
			.filterNot(a => super.hasAuxDataByClass(a.getClass)) // remove any we already have
			.foreach(a => this.storeAuxData(a.copyOnWrite())) // copy the remainder
		super.allAuxData // then return our internal list
	}

	override def removeAuxData[T <: U](clazz: Class[T]): Unit = {
		removedClasses += clazz
		super.removeAuxData(clazz)
	}
	override protected[engine] def storeAuxData(d: U): Unit = {
		removedClasses -= d.getClass
		super.storeAuxData(d)
	}
	override def auxDataOpt[T <: U : Manifest]: Option[T] = {
		if (hasAuxDataByClass(manifest[T].runtimeClass.asInstanceOf[Class[T]])) {
			Some(auxDataByClass(manifest[T].runtimeClass.asInstanceOf[Class[T]]).asInstanceOf[T])
		} else {
			None
		}
	}
	override def hasAuxDataByClass[T <: U](clazz: Class[T]): Boolean =
		base.hasAuxDataByClass(clazz) || super.hasAuxDataByClass(clazz)
}