package arx.engine.control.event

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/16/15
 * Time: 8:22 AM
 */

import arx.Prelude._
import org.lwjgl.glfw.GLFW
import scalaxy.loops._

case class KeyModifiers (ctrl : Boolean, shift : Boolean, alt : Boolean) {}

object KeyModifiers {
	def fromGLFW(mods : Int) = {
		KeyModifiers(
			ctrl = mods.isBitSet(GLFW.GLFW_MOD_CONTROL) || mods.isBitSet(GLFW.GLFW_MOD_SUPER),
			shift = mods.isBitSet(GLFW.GLFW_MOD_SHIFT),
			alt = mods.isBitSet(GLFW.GLFW_MOD_ALT))
	}

	val None = KeyModifiers(false,false,false)

	val Ctrl = KeyModifiers(true,false,false)
	val Shift = KeyModifiers(false,true,false)
	val Alt = KeyModifiers(false,false,true)
}
