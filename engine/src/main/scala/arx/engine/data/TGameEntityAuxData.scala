package arx.engine.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 9:07 AM
 */

import arx.Prelude._
import arx.engine.entity.TGameEntity
import scalaxy.loops._

trait TGameEntityAuxData extends TAuxData {
	def onAssignedToEntity(entity : TGameEntity) {}

	override final def onAssignedToObject(entity: THasAuxData[_]): Unit = {
		entity match {
			case e : TGameEntity =>
				e.world.auxDataAddedToEntity(e, this)
				onAssignedToEntity(e)
			case _ =>
		}
	}
}
