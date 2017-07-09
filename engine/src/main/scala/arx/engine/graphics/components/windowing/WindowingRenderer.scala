package arx.engine.graphics.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.math.Rectf
import arx.core.vec.ReadVec2i
import arx.engine.control.components.windowing.Widget
import arx.engine.graphics.data.WindowingGraphicsData

import scalaxy.loops._

abstract class WindowingRenderer(val windowingData : WindowingGraphicsData) {
	def render (widget : Widget, beforeChildren : Boolean) : Seq[WQuad]

	def intrinsicSize(widget : Widget) : Option[ReadVec2i] = None
	def decorationBorderSize(widget : Widget) : Option[ReadVec2i] = None
}
