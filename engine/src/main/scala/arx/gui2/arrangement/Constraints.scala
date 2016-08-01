package arx.gui2.arrangement

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/1/14
 * Time: 8:23 AM
 */

import arx.core.math.Rectf
import arx.core.vec.ReadVec2f
import arx.core.vec.Vec2f
import arx.gui2.Widget
import arx.core.ImplicitModdable._

class Constraints {
	def gridConstraint (widgets : List[Widget], bounds : Rectf, gap : ReadVec2f): Unit = {
		widgets match {
			case Nil => // do nothing
			case head :: Nil => {
				head.x = bounds.x
				head.y = bounds.y
			}
			case l => {
				val cursor = Vec2f(bounds.x,bounds.y)
				var maxHeightOnLine = 0.0f
				for (widget <- l) {
					val w = widget.width
					val h = widget.height
					if (cursor.x + w > bounds.x + bounds.w) {
						cursor.x = bounds.x
						cursor.y += maxHeightOnLine + gap.y
						maxHeightOnLine = 0.0f
					}
					widget.x = cursor.x
					widget.y = cursor.y
					cursor.x += w + gap.x
					maxHeightOnLine = math.max(maxHeightOnLine,widget.height)
				}
			}
		}
	}

	def listConstraint (widgets : List[Widget], bounds : Rectf, gap : Float): Unit = {
		widgets match {
			case Nil => // do nothing
			case head :: Nil => {
				head.x = bounds.x
				head.y = bounds.y
			}
			case l => {
				val cursor = Vec2f(bounds.x,bounds.y)
				for (widget <- l) {
					val w = widget.width

					widget.x = cursor.x
					widget.y = cursor.y
					cursor.y += widget.height + gap
				}
			}
		}
	}
}