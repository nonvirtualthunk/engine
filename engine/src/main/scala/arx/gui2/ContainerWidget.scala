package arx.gui2

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/9/13
 * Time: 12:24 PM
 */

import arx.Prelude._
import arx.core.Moddable
import arx.core.vec.Vec2f
import arx.core.ImplicitModdable._

trait ContainerWidget extends Widget {
	var autoSize = true
	maxWidth = 5.0f
	maxHeight = 5.0f


	override def width_=(w: Moddable[Float]): Unit = {
		maxWidth = w
		if (autoSize) {}
		else {super.width_=(w)}
	}
	override def height_=(h: Moddable[Float]): Unit = {
		maxHeight = h
		if (autoSize) {}
		else {super.height_=(h)}
	}

	//+====================+ Update +====================+
	abstract override protected[gui2] def updateSubsystem(f: Float): Unit = {
		super.updateSubsystem (f)

		if (autoSize) {
			val min = Vec2f(0.0f,0.0f)
			val max = Vec2f(0.0f,0.0f)
			for ( child <- children ) {
				val x = child.x
				val y = child.y
				val w = child.width
				val h = child.height
				min.x = math.min(x,min.x)
				min.y = math.min(y,min.y)
				max.x = math.max(x+w,max.x)
				max.y = math.max(y+h,max.y)
			}

			val rawWidth = clamp(max.x - min.x + internalPaddingX * 2.0f + interstitialNearX + interstitialFarX,1.0f,maxWidth.resolve())
			val rawHeight = clamp(max.y - min.y + internalPaddingY* 2.0f + interstitialNearY + interstitialFarY,1.0f,maxHeight.resolve())
			desiredWidth = rawWidth
			desiredHeight = rawHeight
		}
	}
}
