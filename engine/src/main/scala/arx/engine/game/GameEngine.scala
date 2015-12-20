package arx.engine.game

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 5:11 PM
 */

import arx.engine.EnginePiece
import arx.engine.game.components.GameComponent
import arx.engine.world.World

class GameEngine(world: World) extends EnginePiece[GameComponent] {
	def graphicsComponents = components

	protected def instantiateComponent(l: List[Class[_]]): AnyRef = {
		l.find(c => try {c.getConstructor(classOf[GameEngine], classOf[World]) != null} catch {case e: Exception => false}) match {
			case Some(clazz) =>
				val constructor = clazz.getConstructor(classOf[GameEngine], classOf[World]);
				constructor.newInstance(this, world).asInstanceOf[GameComponent]
			case None => throw new IllegalStateException(s"Could not instantiate graphics component of possible types $l")
		}
	}
}



