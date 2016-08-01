package arx.engine

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/7/15
 * Time: 7:16 AM
 */

import arx.Prelude._
import arx.core.vec.Vec3f
import arx.engine.TestEngine._
import arx.engine.simple.Canvas
import arx.resource.ResourceManager
import org.lwjgl.glfw.GLFW
import org.lwjgl.opengl.GL11._
import scalaxy.loops._
import EngineCore._

abstract class SimpleEngine extends EngineCore {
	val canvas = new Canvas

	val shader = ResourceManager.shader("shaders/Simple")

	val modelview = arx.graphics.GL.lookAt(Vec3f(0.0f,0.0f,5.0f),Vec3f(0.0f,0.0f,0.0f),Vec3f(0.0f,1.0f,0.0f))
	val projection = arx.graphics.GL.perspective(50.0f,windowWidth/windowHeight.toFloat,0.1f,1000.0f)



	override def draw(): Unit = {
		if (canvas.startDraw()) {
			draw(canvas)
			canvas.finishDraw()
		}

		arx.graphics.GL.glSetState(GL_CULL_FACE, enable = false)
		arx.graphics.GL.glSetState(GL_DEPTH_TEST, enable = false)

		shader.bind()

		shader.setUniform("ModelViewMatrix", modelview, tolerateAbsence = true)
		shader.setUniform("ProjectionMatrix", projection, tolerateAbsence = true)

		canvas.render()
	}

	override def update(deltaSeconds: Float): Unit = {

	}

	override def keyCallback(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
		if (action == GLFW.GLFW_PRESS) {
			keyPressed(key, mods)
		} else if (action == GLFW.GLFW_RELEASE) {
			keyReleased(key, mods)
		}
	}

	def draw (canvas: Canvas)

	def keyPressed (key : Int, mods : Int) {}

	def keyReleased (key : Int, mods : Int) {}
}
