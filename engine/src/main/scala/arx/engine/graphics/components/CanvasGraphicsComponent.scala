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
import arx.core.introspection.ReflectionAssistant
import arx.engine.advanced.lenginecomponents.LGraphicsComponent
import arx.engine.advanced.lenginepieces.LGraphicsEngine
import arx.engine.simple.CustomCanvas
import arx.graphics.GL

abstract class CustomCanvasGraphicsComponent[CanvasType <: CustomCanvas[_] : Manifest](ge : GraphicsEngine) extends GraphicsComponent(ge) {
	var shader = ResourceManager.shader("shaders/Simple")
	val _canvas : CanvasType = ReflectionAssistant.instantiate[CanvasType](manifest[CanvasType])
	var viewport = GL.viewport
	var depthTest = false
	var depthFunc = GL_LESS

	def canvas = _canvas

	def createCanvas = new Canvas

	def updateStarted() {}
	def updateComplete() {}

	def preDraw() {}

	override def update(dt: UnitOfTime): Unit = {
		super.update(dt)

		if (needsUpdate) {
			if (canvas.startDraw()) {
				updateStarted()
				draw(canvas)
				canvas.finishDraw()
				updateComplete()
			}
		}
	}

	override def draw(): Unit = {
		val originalViewport = GL.viewport
		val desiredViewport = canvas.viewportOverride match {
			case Some(ovFunc) => ovFunc(GL.viewport)
			case None => GL.viewport
		}
		GL.pushViewport(desiredViewport)
		viewport = desiredViewport

		arx.graphics.GL.glSetState(GL_CULL_FACE, enable = false)
		arx.graphics.GL.glSetState(GL_DEPTH_TEST, enable = depthTest)
		arx.graphics.GL.glSetDepthFunc(depthFunc)
		arx.graphics.GL.glSetState(GL_BLEND, enable = true)

		shader.bind()

		pov.look()

		preDraw()

		canvas.render()

		GL.popViewport()
	}

	def draw(canvas : CanvasType): Unit
}

// TODO: figure out how to avoid all this stupid duplication
abstract class LCustomCanvasGraphicsComponent[CanvasType <: CustomCanvas[_]](ge : LGraphicsEngine, canvas_ : CanvasType) extends LGraphicsComponent(ge) {
	var shader = ResourceManager.shader("shaders/Simple")
	val _canvas : CanvasType = canvas_
	var viewport = GL.viewport
	var depthTest = false
	var depthFunc = GL_LESS

	def canvas = _canvas

	def createCanvas = new Canvas

	def updateStarted() {}
	def updateComplete() {}

	def preDraw() {}

	override def update(dt: UnitOfTime): Unit = {
		super.update(dt)

		if (needsUpdate) {
			if (canvas.startDraw()) {
				updateStarted()
				draw(canvas)
				canvas.finishDraw()
				updateComplete()
			}
		}
	}

	override def draw(): Unit = {
		val originalViewport = GL.viewport
		val desiredViewport = canvas.viewportOverride match {
			case Some(ovFunc) => ovFunc(GL.viewport)
			case None => GL.viewport
		}
		GL.pushViewport(desiredViewport)
		viewport = desiredViewport

		arx.graphics.GL.glSetState(GL_CULL_FACE, enable = false)
		arx.graphics.GL.glSetState(GL_DEPTH_TEST, enable = depthTest)
		arx.graphics.GL.glSetDepthFunc(depthFunc)
		arx.graphics.GL.glSetState(GL_BLEND, enable = true)

		shader.bind()

		pov.look()

		preDraw()

		canvas.render()

		GL.popViewport()
	}

	def draw(canvas : CanvasType): Unit
}

abstract class CanvasGraphicsComponent(ge:GraphicsEngine) extends CustomCanvasGraphicsComponent[Canvas](ge)