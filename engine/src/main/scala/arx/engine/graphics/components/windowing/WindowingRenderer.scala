package arx.engine.graphics.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.math.Rectf
import arx.core.vec.{ReadVec2f, ReadVec2i}
import arx.engine.control.components.windowing.Widget
import arx.engine.graphics.data.WindowingGraphicsData
import arx.graphics.{AVBO, TextureBlock}
import scalaxy.loops._

abstract class WindowingRenderer(val windowingData : WindowingGraphicsData) {
	def render(widget: Widget, beforeChildren: Boolean, bounds: ReadVec2f, offset: ReadVec2f): Seq[WQuad]
	def renderRaw (vbo: AVBO, textureBlock : TextureBlock, bounds : Rectf, offset : ReadVec2f)(widget : Widget, beforeChildren : Boolean) {}
	def renderCustomVBO (textureBlock : TextureBlock, bounds : Rectf, offset : ReadVec2f)(widget : Widget) : Option[AVBO] = None

	def intrinsicSize(widget: Widget, fixedX: Option[Int], fixedY: Option[Int]): Option[ReadVec2i] = None
	def decorationBorderSize(widget : Widget) : Option[ReadVec2i] = None
}
