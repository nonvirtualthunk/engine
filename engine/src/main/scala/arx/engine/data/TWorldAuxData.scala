package arx.engine.data

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 12/19/15
  * Time: 12:45 PM
  */

import arx.application.Noto
import arx.core.introspection.CopyAssistant
import arx.engine.world.World

trait TWorldAuxData extends TAuxData {

	@transient protected var _world: World = World.Sentinel

	def world = _world

	def world_=(w: World) { _world = w; onWorldAssigned(w) }

	def onWorldAssigned(world: World) {}

	final override def onAssignedToObject(entity: THasAuxData[_]): Unit = entity match {
		case world: World => {
			_world = world
			onWorldAssigned(world)
		}
		case _ => Noto.severeError("World data assigned to non-world")
	}
}



