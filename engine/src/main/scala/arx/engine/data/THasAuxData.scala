package arx.engine.data

import arx.core.introspection.ReflectionAssistant

import scala.collection.mutable

trait THasAuxData[U <: TAuxData] {
	def auxData[T <: U] (clazz : Class[T]) : T
	def auxData[T <: U : Manifest]: T = auxData(manifest[T].runtimeClass.asInstanceOf[Class[T]])

	protected def onNewAuxDataCreated(gead: U) {}

	def auxDataOrElse[T <: U : Manifest](orElse: T): T

	def allAuxData: List[U]

	/** Returns an auxData that descends from the given type, if one exists */
	def auxDataWithTrait[T: Manifest] = allAuxData.find(v => manifest[T].runtimeClass.isAssignableFrom(v.getClass)).asInstanceOf[Option[T]]

	def removeAuxData[T <: U : Manifest]() { removeAuxData(manifest[T].runtimeClass.asInstanceOf[Class[T]]) }

	def removeAuxData[T <: U](clazz: Class[T])

	def manualAddAuxData(d: U): Unit = {
		addAuxData(d)
	}

	protected def addAuxData(d: U): Unit = {
		onNewAuxDataCreated(d)
		d.onAssignedToObject(this)
		storeAuxData(d)
	}

	protected def storeAuxData(d: U)

	def aux[T <: U : Manifest] = auxData[T]

	def ->[T <: U] (classWrap: AuxDataObject[T]) = auxData(classWrap.clazz)

	def auxDataOpt[T <: U : Manifest]: Option[T]

	def hasAuxData[T <: U : Manifest]: Boolean
}


