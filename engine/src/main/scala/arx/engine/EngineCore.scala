package arx.engine

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 12/5/15
  * Time: 8:52 AM
  */

import java.util.concurrent.locks.LockSupport

import arx.Prelude.int2RicherInt
import arx.application.Application
import arx.application.Noto
import arx.core.async.{Executor, OnExitRegistry}
import arx.core.datastructures.KillableThread
import arx.core.introspection.NativeLibraryHandler
import arx.core.math.Recti
import arx.core.metrics.Metrics
import arx.core.vec.Vec4f
import arx.core.vec.{Vec2f, Vec2i}
import arx.engine.control.event.Event.KeyboardMirror
import arx.engine.control.event.Event.Mouse
import arx.engine.control.event.Event.MouseButton
import org.lwjgl.BufferUtils
import org.lwjgl.glfw.GLFW._
import org.lwjgl.glfw._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl._
import org.lwjgl.system.MemoryUtil
import org.lwjgl.system.MemoryUtil._


abstract class EngineCore {
	var errorCallback: GLFWErrorCallback = null
	var keyCallback: GLFWKeyCallback = null
	var mouseButtonCallbackIntern: GLFWMouseButtonCallback = null
	var mouseMoveCallbackIntern: GLFWCursorPosCallback = null
	var scrollCallbackIntern: GLFWScrollCallback = null
	var charCallbackIntern: GLFWCharCallback = null
	var sizeCallback: GLFWWindowSizeCallback = null
	var focusCallback: GLFWWindowFocusCallback = null

	// The window handle
	var window = 0l

	var fullscreen = false
	var hasFocus = true
	var fullPause = false

	var mouseGrabbed = false

	var clearColor = Vec4f(0.0f,0.0f,0.0f,1.0f)


	def run(): Unit = {
		println("GLFW Version: " + GLFW.glfwGetVersionString());
		System.setProperty("java.awt.headless", "true")
		try {
			init()
			loop()
		} finally {
			onShutdown()
			KillableThread.kill()
			Executor.onQuit()
			OnExitRegistry.onExit()
			// Release window and window callbacks
			glfwDestroyWindow(window)
			if (keyCallback != null) { keyCallback.free() }

			glfwTerminate()
			if (errorCallback != null) {
				errorCallback.free()
			}
		}
	}

	def onShutdown() {}

	def init(): Unit = {
		NativeLibraryHandler.load()

		// Setup an error callback. The default implementation
		// will print the error message in System.err.
		errorCallback = GLFWErrorCallback.createPrint(System.err)
		glfwSetErrorCallback(errorCallback)

		// Initialize GLFW. Most GLFW functions will not work before doing this.
		if (!glfwInit()) {
			throw new IllegalStateException("Unable to initialize GLFW")
		}

		// Configure our window
		glfwDefaultWindowHints() // optional, the current window hints are already the default
//		glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE) // the window will stay hidden after creation
//		glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE) // the window will be resizable

		glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE)
		glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE)
		glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
		glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2)
//		glfwWindowHint(GLFW_STENCIL_BITS, 8)


		// Create the window
		if (fullscreen) {
			val vidMode = glfwGetVideoMode(glfwGetPrimaryMonitor())
			EngineCore.windowWidth = vidMode.width
			EngineCore.windowHeight = vidMode.height
			window = glfwCreateWindow(EngineCore.windowWidth, EngineCore.windowHeight, "Hello World!", glfwGetPrimaryMonitor(), NULL)
		} else {
			window = glfwCreateWindow(EngineCore.windowWidth, EngineCore.windowHeight, "Hello World!", NULL, NULL)
		}

		updateWindowSize()


		if (window == NULL) {
			throw new RuntimeException("Failed to create the GLFW window")
		}

		Mouse.windowRef = window

		// Setup a key callback. It will be called every time a key is pressed, repeated or released.
		keyCallback = new GLFWKeyCallback() {
			def invoke(window: Long, key: Int, scancode: Int, action: Int, mods: Int) {
				if (key == GLFW_KEY_Q && (mods.isBitSet(GLFW_MOD_CONTROL) || mods.isBitSet(GLFW_MOD_SUPER))) {
					glfwSetWindowShouldClose(window, true)
				} else if (key == GLFW_KEY_F3 && mods.isBitSet(GLFW_MOD_SHIFT) && action == GLFW_PRESS) {
					Metrics.prettyPrint()
				} else if (key == GLFW_KEY_F5 && action == GLFW_PRESS) {
					fullPause = ! fullPause
				}

				if (action == GLFW_PRESS) {
					KeyboardMirror.setKeyDown(key, isDown = true)
				} else if (action == GLFW_RELEASE) {
					KeyboardMirror.setKeyDown(key, isDown = false)
				}
				keyCallback(key, scancode, action, mods)
			}
		}
		glfwSetKeyCallback(window, keyCallback)

		mouseButtonCallbackIntern = new GLFWMouseButtonCallback {
			override def invoke(window: Long, buttonRaw: Int, action: Int, mods: Int): Unit = {
				val button = buttonRaw match {
					case GLFW_MOUSE_BUTTON_LEFT => MouseButton.Left
					case GLFW_MOUSE_BUTTON_RIGHT => MouseButton.Right
					case GLFW_MOUSE_BUTTON_MIDDLE => MouseButton.Middle
					case _ => MouseButton.Other
				}
				action match {
					case GLFW_PRESS => Mouse.buttonDown += button -> true
					case GLFW_RELEASE => Mouse.buttonDown += button -> false
				}

				mouseButtonCallback(button, action, mods)
			}
		}
		glfwSetMouseButtonCallback(window, mouseButtonCallbackIntern)

		mouseMoveCallbackIntern = new GLFWCursorPosCallback {
			override def invoke(window: Long, x: Double, y: Double): Unit = {
				Mouse.currentPosition = Vec2f(x.toFloat, y.toFloat)
				mousePosCallback(x.toFloat, y.toFloat)
			}
		}
		glfwSetCursorPosCallback(window, mouseMoveCallbackIntern)

		scrollCallbackIntern = new GLFWScrollCallback {
			override def invoke(window: Long, dx: Double, dy: Double): Unit = {
				scrollCallback(dx.toFloat, dy.toFloat)
			}
		}
		glfwSetScrollCallback(window, scrollCallbackIntern)

		charCallbackIntern = new GLFWCharCallback {
			override def invoke(window: Long, codePoint: Int): Unit = {
				val str = String.valueOf(Character.toChars(codePoint))
				charCallback(str)
			}
		}
		glfwSetCharCallback(window, charCallbackIntern)


		sizeCallback = new GLFWWindowSizeCallback {
			override def invoke(window: Long, width: Int, height: Int): Unit = {
				EngineCore.windowWidth = width
				EngineCore.windowHeight = height
				updateWindowSize()
				windowSizeCallback(width, height)
			}
		}
		glfwSetWindowSizeCallback(window, sizeCallback)

		focusCallback = new GLFWWindowFocusCallback {
			override def invoke(l: Long, focus: Boolean): Unit = {
				hasFocus = focus
			}
		}
		glfwSetWindowFocusCallback(window, focusCallback)

		if (!fullscreen) {
			// Get the resolution of the primary monitor
			val vidmode = glfwGetVideoMode(glfwGetPrimaryMonitor())

			// Center our window
			glfwSetWindowPos(
				window,
				(vidmode.width() - EngineCore.windowWidth) / 2,
				(vidmode.height() - EngineCore.windowHeight) / 2
			)
		}

		// Make the OpenGL context current
		glfwMakeContextCurrent(window)
		// Enable v-sync
		glfwSwapInterval(0)

//		// Make the window visible
		glfwShowWindow(window)
	}

	def keyCallback(key: Int, scancode: Int, action: Int, mods: Int): Unit = {

	}

	def charCallback(str: String): Unit = {

	}

	def mouseButtonCallback(button: MouseButton, action: Int, mods: Int): Unit = {

	}

	def mousePosCallback(x: Float, y: Float): Unit = {

	}

	def scrollCallback(dx: Float, dy: Float): Unit = {

	}

	def windowSizeCallback(width: Int, height: Int) = {

	}

	def updateWindowSize(): Unit = {
		val xb = BufferUtils.createIntBuffer(1)
		val yb = BufferUtils.createIntBuffer(1)
		glfwGetWindowSize(window,xb, yb)
		println(s"Current window size: ${xb.get(0)}, ${yb.get(0)}")
		glfwGetFramebufferSize(window, xb, yb)
		println(s"Frame buffer size: ${xb.get(0)}, ${yb.get(0)}")
		EngineCore.pixelWidth = xb.get(0)
		EngineCore.pixelHeight = yb.get(0)
	}

	def desiredViewportSize = {
		Vec2i(EngineCore.pixelWidth, EngineCore.pixelHeight)
	}

	def loop(): Unit = {
		GL.createCapabilities()

		Application.openGLThread.set(true)

		// Set the clear color
		glClearColor(clearColor.r,clearColor.g,clearColor.b,clearColor.a)

		arx.graphics.GL.maximumViewport = Recti(0, 0, desiredViewportSize.x, desiredViewportSize.y)
		arx.graphics.GL.setViewport(Recti(0, 0, desiredViewportSize.x, desiredViewportSize.y))

		arx.graphics.GL.glSetState(GL_BLEND,enable = true)
		glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)

		var lastUpdated = GLFW.glfwGetTime()

		// Run the rendering loop until the user has attempted to close
		// the window or has pressed the ESCAPE key.
		while (!glfwWindowShouldClose(window)) {
			if (hasFocus && ! fullPause) {
				glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT) // clear the framebuffer

				if (arx.graphics.GL.viewportSize != desiredViewportSize) {
					arx.graphics.GL.maximumViewport = Recti(0, 0, desiredViewportSize.x, desiredViewportSize.y)
					arx.graphics.GL.setViewport(Recti(0, 0, desiredViewportSize.x, desiredViewportSize.y))
				}
			}

			val curTime = GLFW.glfwGetTime()
			val deltaSeconds = curTime - lastUpdated
			lastUpdated = curTime
			if (deltaSeconds > (0.016666667 * 1.25)) {
//				Noto.info("Long update time: " + deltaSeconds)
			}

			if (!fullPause) {
				update(deltaSeconds.toFloat)
			}

			if (hasFocus && ! fullPause) {
				draw()

				glfwSwapBuffers(window) // swap the color buffers
			}

			// Poll for window events. The key callback above will only be
			// invoked during this call.
			glfwPollEvents()
			if (!hasFocus || fullPause) {
				LockSupport.parkNanos((0.1 * 1e9f).toLong) // wait a 60th of a second
			}
		}
	}


	def update(deltaSeconds: Float)

	def draw()

	def scalaMain(args: Array[String]) {
		val moduleField = this.getClass.getField("MODULE$")
		if (moduleField != null) {
			moduleField.get(null).asInstanceOf[EngineCore].run()
		} else {
			Noto.error("Could not find module field to run")
		}
	}
}
object EngineCore {
	var windowWidth = 800
	var windowHeight = 600
	var pixelWidth = windowWidth
	var pixelHeight = windowHeight

	def pixelScaleFactor = {
		EngineCore.pixelWidth.toFloat / EngineCore.windowWidth.toFloat
	}
}