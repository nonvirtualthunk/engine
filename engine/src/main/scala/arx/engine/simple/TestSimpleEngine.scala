package arx.engine.simple

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/7/15
 * Time: 7:52 AM
 */

import arx.Prelude._
import arx.core.vec.Vec4f
import arx.engine.SimpleEngine
import arx.resource.ResourceManager
import scalaxy.loops._

object TestSimpleEngine extends SimpleEngine {
	var degrees = 0.0f

	override def draw(canvas: Canvas): Unit = {
//		canvas.drawQuad(0.0f,0.0f,5.0f,5.0f,ResourceManager.image("default/defaultium.png"))
		canvas.drawQuad(0.0f,0.0f,5.0f,5.0f,degrees,Vec4f.One,ResourceManager.image("default/defaultium.png"))
	}

	override def update(deltaSeconds: Float): Unit = {
		degrees += 0.5f
	}



	override def keyPressed(key: Int, mods: Int): Unit = {}

	override def keyReleased(key: Int, mods: Int): Unit = {}

	def main(args: Array[String]) {
		scalaMain(args)
	}
}
