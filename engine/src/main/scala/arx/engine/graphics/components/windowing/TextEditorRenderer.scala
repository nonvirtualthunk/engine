package arx.engine.graphics.components.windowing

import arx.core.math.Rectf
import arx.core.vec.{ReadVec2f, Vec2f, Vec4f}
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.{TextDisplayRenderedGlyphData, TextEditorWidget}
import arx.engine.graphics.data.WindowingGraphicsData
import arx.resource.ResourceManager

class TextEditorRenderer(WD: WindowingGraphicsData) extends WindowingRenderer(WD) {
	override def render(widget: Widget, beforeChildren: Boolean, bounds: ReadVec2f, absOffset: ReadVec2f): Seq[WQuad] = {
		widget match {
			case tew: TextEditorWidget =>
				if (tew.cursorShowing && ! beforeChildren) {
					val GD = tew.textDisplay[TextDisplayRenderedGlyphData]
					val offset = Vec2f(tew.textDisplay.drawing.absolutePosition.xy) - absOffset
					val rects = GD.glyphRects
					val cursorPos = if (rects.isEmpty) {
						Vec2f.Zero
					} else {
						tew.cursorIndex match {
							case -1 => Vec2f(0.0f, 0.0f)
							case i if i >= rects.size => Vec2f(rects.last.maxX, rects.last.y)
							case i => Vec2f(rects(i).x - 2, rects(i).y)
						}
					}

					Seq(WQuad(Rectf(offset.x + cursorPos.x, offset.y + cursorPos.y, 4, GD.lineHeight),
						ResourceManager.image("default/blank.png"),
						Vec4f(0.5f, 0.5f, 0.6f, 1.0f)))
				} else {
					Nil
				}
			case _ => Nil
		}
	}
}
