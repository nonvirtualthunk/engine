package arx.engine.requirement

import arx.engine.data.TGameEntityAuxData
import arx.engine.entity.TGameEntity

class AuxDataDescriptor[T <: TGameEntityAuxData : Manifest](f : (T) => Boolean) extends EntityDescriptor {
	val filter = f
	override def matchesEntity(entity: TGameEntity): Boolean = entity.hasAuxData[T] && filter(entity[T])

	override def toString = s"AuxDataDescriptor[${manifest[T].runtimeClass.getSimpleName}]"
}

object AuxDataDescriptor {
	def apply[T <: TGameEntityAuxData : Manifest] = new AuxDataDescriptor[T]((x : T) => true)
	def apply[T <: TGameEntityAuxData : Manifest] (f : (T) => Boolean) : AuxDataDescriptor[T] = new AuxDataDescriptor[T](f)
}
