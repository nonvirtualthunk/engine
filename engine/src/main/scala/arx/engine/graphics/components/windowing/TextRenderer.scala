package arx.engine.graphics.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.math.Rectf
import arx.core.vec.ReadVec2i
import arx.core.vec.Vec2i
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.TextDisplayWidget
import arx.engine.graphics.data.WindowingGraphicsData
import arx.graphics.Image
import arx.graphics.helpers.Color
import arx.graphics.helpers.TTextLayouter
import arx.graphics.helpers.TTextLayouter.LayoutParameters
import arx.graphics.helpers.TextLayouter
import arx.graphics.text.TBitmappedFont

import scalaxy.loops._

class TextRenderer(WD : WindowingGraphicsData) extends WindowingRenderer(WD) {
	val layouter : TTextLayouter = PixelLayouter

	def effectiveFontFor(tw : TextDisplayWidget) = tw.font match {
		case Some(wrapper) => wrapper.font(WD.textureBlock)
		case None => WD.defaultFont
	}

	def layout(tw : TextDisplayWidget) = {
		import tw._
		val area = Rectf(0.0f,0.0f,10000.0f,100000.0f)
		layouter.layOutText(LayoutParameters(text, effectiveFontFor(tw), effectiveFontScale, area, 1.0f, effectiveFontScale/2, textAlignment))
	}

	override def render(widget: Widget, beforeChildren: Boolean): List[WQuad] = {
		widget match {
			case tw : TextDisplayWidget =>
				import tw._
				val effFont = effectiveFontFor(tw)
				val res = layout(tw)
				res.points.zip(text.resolve()).flatMap( t => {
					val (point,char) = t
					if ( ! char.isWhitespace ) {
						val rawW = layouter.charWidth(char,effFont,effectiveFontScale)
						val rawH = layouter.charHeight(char,effFont,effectiveFontScale)
						val w = floorf( rawW + 0.0001f)
						val h = floorf( rawH + 0.0001f)

						val x = point.x
						val y = point.y
//	TODO: bounds culling optimization
//							if ( y < bounds.y + bounds.h && y + h > bounds.y ) {
							val tc = effFont.characterTexCoords(char)
//
//								val tx = tc(0).x// + 100.0f
//								val ty = tc(0).y// + 100.0f
//								val tw = tc(2).x - tc(0).x
//								val th = tc(2).y - tc(0).y

//							} else { break = true }
						List(WQuad(Rectf(x,y,w,h),Image.Sentinel,Color.Black,0,WQuad.StandardRect, Some(tc)))
					} else {
						Nil
					}
				})
			case _ => Nil
		}
	}

	override def intrinsicSize(widget: Widget): Option[ReadVec2i] = {
		widget match {
			case tw : TextDisplayWidget =>
				val points = layout(tw)
				Some(Vec2i(points.dimensions))
			case _ => None
		}
	}
}


object PixelLayouter extends TextLayouter {
	override def charWidth(char: Char, font: TBitmappedFont, fastFontSize: Float): Float = {
		font.characterWidthPixels(char) * fastFontSize
	}

	override def charHeight(char: Char, font: TBitmappedFont, fastFontSize: Float): Float = {
		font.characterHeightPixels(char) * fastFontSize
	}

	override def lineHeight(font: TBitmappedFont, fontSize: Float): Float = font.maxCharacterDimensionsPixels.y * fontSize

	override def spaceSize(fastFontSize: Float): Float = fastFontSize * 4
}