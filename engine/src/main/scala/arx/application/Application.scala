package arx.application

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/5/15
 * Time: 10:42 AM
 */

import arx.Prelude._
import org.lwjgl.glfw.GLFW

import scalaxy.loops._

object Application {
	var ticks = 0
	var openGLThread = new ThreadLocal[Boolean]{override def initialValue() = false}

	def isOpenGLThread() = {
		openGLThread.get()
	}
}
