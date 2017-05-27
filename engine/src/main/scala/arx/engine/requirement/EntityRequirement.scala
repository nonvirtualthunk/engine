package arx.engine.requirement

/**
 * TODO: Add javadoc
 */

import arx.engine.entity.TGameEntity

trait EntityRequirement extends Requirement {
	def amountSatisfiedByEntity(entity : TGameEntity) : Int
	override final def amountSatisfiedBy(entity: Any): Int = {
		entity match {
			case ge : TGameEntity => amountSatisfiedByEntity(ge)
			case _ => 0
		}
	}
}
