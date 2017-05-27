package arx.engine.game.events

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.engine.control.event.Event.Event
import arx.engine.entity.TGameEntity

import scalaxy.loops._

case class EntityAddedEvent(entity : TGameEntity) extends Event {
}

case class EntityRemovedEvent(entity : TGameEntity) extends Event {

}