package arx.engine.advanced

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/16/15
 * Time: 8:11 AM
 */

import arx.Prelude._
import arx.control.ControlEngine
import arx.core.vec.ReadVec2f
import arx.engine.EngineCore
import arx.engine.control.event.Event._
import arx.engine.event.EventBus
import arx.engine.game.GameEngine
import arx.engine.graphics.GraphicsEngine
import arx.engine.world.World
import arx.gui2.WindowingSystem2
import org.lwjgl.glfw.GLFW

import scalaxy.loops._

abstract class Engine extends EngineCore with TEventUser {
	val world = new World
	val gameEventBus = new EventBus
	val graphicsEventBus = new EventBus
	val gameEngine = new GameEngine(world, gameEventBus)
	val graphicsEngine = new GraphicsEngine(world, graphicsEventBus, gameEventBus)
	val windowingSystem = new WindowingSystem2(this)
	val controlEngine = new ControlEngine(world, graphicsEventBus, gameEventBus, graphicsEngine, windowingSystem)

	var first = true

	def setUpEngine()


	override def init(): Unit = {
		super.init()

		setUpEngine();
	}

	override def update(deltaSeconds: Float): Unit = {
		gameEngine.update(deltaSeconds)
		graphicsEngine.update(deltaSeconds)
		windowingSystem.update(deltaSeconds)
		controlEngine.update(deltaSeconds)
	}

	override def draw(): Unit = {
		graphicsEngine.draw()
		windowingSystem.draw()
	}

	eventFallback {
		case e : Event =>
			windowingSystem.handleEvent(e)
			if (e.notConsumed) {
				controlEngine.handleEvent(e)
				if (e.notConsumed) {
					graphicsEngine.pov.handleEvent(e)
				}
			}
	}

	// Transform callbacks into event objects
	override def keyCallback(key: Int, scancode: Int, action: Int, mods: Int): Unit = {
		val keyMods = KeyModifiers.fromGLFW(mods)
		val event = action match {
			case GLFW.GLFW_PRESS => KeyPressEvent(key, keyMods)
			case GLFW.GLFW_RELEASE => KeyReleaseEvent(key, keyMods)
			case GLFW.GLFW_REPEAT => KeyPressEvent(key, keyMods, isRepeat = true)
		}
		this.handleEvent(event)
	}

	override def charCallback(str: String): Unit = this.handleEvent(CharEnteredEvent(str))

	override def mouseButtonCallback(button: MouseButton, action: Int, mods: Int): Unit = {
		val keyMods = KeyModifiers.fromGLFW(mods)
		val event = action match {
			case GLFW.GLFW_PRESS => MousePressEvent(button, Mouse.currentPosition, keyMods)
			case GLFW.GLFW_RELEASE => MouseReleaseEvent(button, Mouse.currentPosition, keyMods)
		}
		this.handleEvent(event)
	}

	override def mousePosCallback(x: Float, y: Float): Unit = {
		val keyMods = KeyboardMirror.activeModifiers
		val event = if (Mouse.buttonDown.exists(t => t._2)) {
			val buttons = Mouse.buttonDown.filter(t => t._2).keys.toSet
			MouseDragEvent(Mouse.currentPosition, Mouse.currentPosition - Mouse.previousPosition, buttons, keyMods)
		} else {
			MouseMoveEvent(Mouse.currentPosition, Mouse.currentPosition - Mouse.previousPosition, keyMods)
		}
		this.handleEvent(event)
	}

	override def scrollCallback(dx: Float, dy: Float): Unit = {
		val event = ScrollEvent(ReadVec2f(dx,dy), KeyboardMirror.activeModifiers)
		this.handleEvent(event)
	}

	def main (args : Array[String]): Unit = {
		scalaMain(args)
	}
}
