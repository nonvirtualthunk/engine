package arx.engine.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 8:46 AM
 */

import arx.Prelude._
import arx.core.introspection.ReflectionAssistant
import scalaxy.loops._

trait THasExternalAuxData[U <: TAuxData] extends THasAuxData[U] {
	def id : Long

	def externalStore : TExternalAuxDataStore[U]

	override def auxData[T <: U](clazz: Class[T]): T = {
		val existing = externalStore.getAuxDataOrElse(id, clazz, null.asInstanceOf[T])
		existing match {
			case null =>
				val newData = ReflectionAssistant.instantiate(clazz)
				addAuxData(newData)
				newData
			case nonNull => nonNull
		}
	}

	override protected[engine] def storeAuxData(d: U): Unit = externalStore.storeAuxData(id, d)

	override def hasAuxData[T <: U : Manifest]: Boolean = {
		externalStore.getAuxDataOrElse[T](id, manifest[T].runtimeClass.asInstanceOf[Class[T]], null.asInstanceOf[T]) == null.asInstanceOf[T]
	}

	override def removeAuxData[T <: U](clazz: Class[T]): Unit = externalStore.removeAuxData[T](id, clazz.asInstanceOf[Class[T]])

	override def auxDataOpt[T <: U: Manifest]: Option[T] = {
		externalStore.getAuxDataOrElse(id, manifest[T].runtimeClass.asInstanceOf[Class[T]], null.asInstanceOf[T]) match {
			case null => None
			case nonNull => Some(nonNull)
		}
	}

	override def allAuxData: List[U] = externalStore.allAuxData(id)

	override def auxDataOrElse[T <: U : Manifest](orElse: T): T = auxDataOpt[T].getOrElse(orElse)
}

