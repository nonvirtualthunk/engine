package arx.gui2.rendering

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/4/13
 * Time: 1:41 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.core.Moddable
import arx.core.vec._
import arx.graphics.AVBO
import arx.graphics.helpers.Color
import arx.gui2.Widget
import arx.gui2.WindowingSystem2.RenderingContext
import Cardinals._
import arx.resource.ResourceManager

class TextEditorWidgetRenderingComponent ( textRenderer : WidgetTextRenderingComponent, cursorIndex : Moddable[Int], selection : Moddable[Option[(Int,Int)]], showCursor : Moddable[Boolean] ) extends WidgetRenderingComponent {
	def cursorPosition = { positionForIndex(cursorIndex) }

	def positionForIndex ( idx : Int ) = {
		val layout = textRenderer.currentLayout
		if ( layout.points.isEmpty ) {
			if (textRenderer.textAlignment.resolve() == Center) {
				Vec2f(textRenderer.area.x + textRenderer.area.w * 0.5f,
						textRenderer.area.y + (textRenderer.area.h - textRenderer.lineHeight) * 0.5f)
			} else {
				Vec2f(textRenderer.area.x,textRenderer.area.y)
			}
		} else if ( idx >= layout.points.size ) {
			val textString : String = textRenderer.text
			if ( textString.nonEmpty ) {
				layout.points.last + Vec2f(textRenderer.layouter.charWidth(textString.charAt( textString.length - 1 ),textRenderer.font,textRenderer.fontSize),0.0f)
			} else { Vec2f.Zero }

		} else {
			layout.points(idx)
		}
	}

	var mostRecentCursorPosition : ReadVec2f = Vec2f.Zero
	var timeStationary = 0

	def draw(widget: Widget, vbo: AVBO, context: RenderingContext, beforeChildren: Boolean) = {
		if ( beforeChildren ) {
			widget.selfBoundedBlock(context) {
				val position = cursorPosition
				val tc = context.textureBlock( ResourceManager.image("default/blank.png") )

				if ( position == mostRecentCursorPosition ) {
					timeStationary += 1
				} else {
					mostRecentCursorPosition = position
					timeStationary = 0
				}

				selection.resolve() match {
					case Some((start,end)) if start != end => {
						var selectedIndex = start
						var startMarkX = positionForIndex(selectedIndex).x
						var markX = startMarkX
						var markY = positionForIndex(selectedIndex).y

						while ( selectedIndex <= end+1 ) {
							val pos = positionForIndex(selectedIndex)

							if ( markY != pos.y || selectedIndex == end+1 ) {
								drawQuad(vbo,context,
									floorf(cx(context,startMarkX)) - 1,
									floorf(cy(context,markY)) - 1,
									ceilf(cx(context,markX - startMarkX))+2,
									ceilf(cy(context,textRenderer.lineHeight)),
									tc,Vec4f(0.3f,0.4f,0.5f,1.0f)
								)
								startMarkX = pos.x
								markY = pos.y
							}
							markX = positionForIndex(selectedIndex).x

							selectedIndex += 1
						}
					}
					case _ =>
				}

				if ( showCursor && (! widget.hasFocus || (timeStationary / 60) % 2 == 0) ) {
					val textString : String = textRenderer.text
					val mx = -1.0f//if (cursorIndex >= textString.length || textString(cursorIndex-1) == ' ') { 0.5f } else { -0.5f }
					val x = context.toPixelScaleX(position.x) + mx
					val y = floorf(context.toPixelScaleY(position.y)) - 1
					drawQuad(vbo,context,x,y,2,ceilf(context.toPixelScaleY(textRenderer.lineHeight)) + 1,tc,Color.Black)
				}

			}
		}
	}
}
