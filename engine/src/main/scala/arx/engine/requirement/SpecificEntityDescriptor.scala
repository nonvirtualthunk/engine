package arx.engine.requirement

import arx.engine.entity.EntityReference
import arx.engine.entity.TGameEntity
import arx.engine.world.World

/**
  * TODO: Add javadoc
  */

case class SpecificEntityDescriptor(entity:TGameEntity) extends EntityDescriptor{
	override def matchesEntity(entity: TGameEntity): Boolean = this.entity == entity

	override def isSubsetOf(other: Descriptor): Boolean = {
		// if the specific entity would also match this other descriptor then this is by definition
		// either a subset (or a twin) of the other descriptor
		other.matches(entity)
	}
}

object SpecificEntityDescriptor {
//	def apply (ent : TGameEntity) = new SpecificEntityDescriptor(ent)
	def apply (ent : EntityReference) = SpecificEntityReferenceDescriptor(ent)
}

case class SpecificEntityReferenceDescriptor(ent: EntityReference) extends EntityDescriptor {
	override def matchesEntity(entity: TGameEntity): Boolean = entity.id == ent.id

	override def resolve(world: World) = SpecificEntityDescriptor(ent.in(world))
}