package arx.engine.requirement

/**
  * TODO: Add javadoc
  */

object AnyOneRequirement extends Requirement {
	amount = 1
	override def amountSatisfiedBy(entity: Any): Int = 1
}
