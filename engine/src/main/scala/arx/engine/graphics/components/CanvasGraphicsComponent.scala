package arx.engine.graphics.components

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 12:07 PM
 */

import arx.core.units.UnitOfTime
import arx.engine.graphics.GraphicsEngine
import arx.engine.simple.Canvas
import arx.engine.world.World
import arx.resource.ResourceManager
import org.lwjgl.opengl.GL11._
import arx.Prelude._

abstract class CanvasGraphicsComponent(ge : GraphicsEngine, world : World) extends GraphicsComponent(ge,world) {
	val shader = ResourceManager.shader("shaders/Simple")
	val canvas = new Canvas

	override def update(dt: UnitOfTime): Unit = {
		if (canvas.startDraw()) {
			draw(canvas)
			canvas.finishDraw()
		}
	}

	override def draw(): Unit = {
		arx.graphics.GL.glSetState(GL_CULL_FACE, enable = false)
		arx.graphics.GL.glSetState(GL_DEPTH_TEST, enable = false)

		shader.bind()

		pov.look()

		canvas.render()
	}

	def draw(canvas : Canvas): Unit
}
