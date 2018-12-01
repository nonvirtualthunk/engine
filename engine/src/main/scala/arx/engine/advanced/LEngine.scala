package arx.engine.advanced

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/17/18
  * Time: 7:59 AM
  */

import arx.Prelude._
import scalaxy.loops._
import arx.core.vec._
import arx.engine.EngineCore
import arx.engine.advanced.lenginepieces.{LControlEngine, LGameEngine, LGraphicsEngine}
import arx.engine.control.ControlEngine
import arx.engine.control.data.ControlWorld
import arx.engine.control.event.Event._
import arx.engine.event.EventBus
import arx.engine.game.GameEngine
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.data.GraphicsWorld
import arx.engine.lworld.LWorld
import arx.engine.world.World
import arx.resource.ResourceManager
import org.lwjgl.glfw.GLFW

abstract class LEngine  extends EngineCore with TEventUser {
	val world = new LWorld
	val graphicsWorld = new GraphicsWorld
	val controlWorld = new ControlWorld
	val gameEventBus = new EventBus
	val graphicsEventBus = new EventBus
	val controlEventBus = new EventBus
	val gameEngine = new LGameEngine(world, gameEventBus)
	val graphicsEngine : LGraphicsEngine = new LGraphicsEngine(world, graphicsWorld, graphicsEventBus, gameEventBus)
	val controlEngine : LControlEngine = new LControlEngine(world, graphicsWorld, controlWorld, controlEventBus, graphicsEventBus, gameEventBus)
	var serialGameEngine = false
	var serialGraphicsEngine = false
	var serialControlEngine = false
	def engines = gameEngine :: graphicsEngine :: controlEngine :: Nil

	var first = true

	def setUpEngine()


	override def init(): Unit = {
		super.init()

		setUpEngine()

		var context = gameEngine.resolveComponents(engines)
		context = graphicsEngine.resolveComponents(context)
		context = controlEngine.resolveComponents(context)

		gameEngine.initialize(serialGameEngine)
		graphicsEngine.initialize(serialGraphicsEngine)
		controlEngine.initialize(serialControlEngine)
	}

	override def update(deltaSeconds: Float): Unit = {
		if (!serialGameEngine) {
			gameEngine.update(deltaSeconds)
		} else {
			gameEngine.updateSerial(deltaSeconds)
		}

		if (!serialGraphicsEngine) {
			graphicsEngine.update(deltaSeconds)
		} else {
			graphicsEngine.updateSerial(deltaSeconds)
		}

		if (!serialControlEngine) {
			controlEngine.update(deltaSeconds)
		} else {
			controlEngine.updateSerial(deltaSeconds)
		}
	}

	override def draw(): Unit = {
		graphicsEngine.draw()
	}

	eventFallback {
		case KeyPressEvent(key,_,_) if key == GLFW.GLFW_KEY_F2 =>
			ResourceManager.refreshImages()
			ResourceManager.reloadShaders()
		case e : Event => controlEngine.handleEvent(e)
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
