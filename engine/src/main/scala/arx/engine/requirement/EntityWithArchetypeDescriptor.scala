package arx.engine.requirement

import arx.engine.entity.GameArchetype
import arx.engine.entity.TGameEntity

case class EntityWithArchetypeDescriptor (archetype : GameArchetype) extends EntityDescriptor {
	override def matchesEntity(entity: TGameEntity): Boolean = entity.archetype.contains(archetype)
}
