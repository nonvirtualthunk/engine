package arx.engine.requirement

import arx.engine.entity.GameArchetype
import arx.engine.entity.TGameEntity

/**
  * TODO: Add javadoc
  */

/**
  * Requirement that selects for only an individual entity, as opposed to an archetype
  */
object IndividualEntityDescriptor extends EntityDescriptor {
	override def matchesEntity(entity: TGameEntity): Boolean = {
		entity match {
			case ga : GameArchetype => false
			case _ => true
		}
	}
}
