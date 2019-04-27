package arx.engine.graphics.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.math.Rectf
import arx.core.vec.ReadVec2i
import arx.core.vec.Vec2i
import arx.engine.EngineCore
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.TextDisplayWidget
import arx.engine.graphics.data.WindowingGraphicsData
import arx.graphics.Image
import arx.graphics.helpers._
import arx.graphics.helpers.TTextLayouter.LayoutParameters
import arx.graphics.text.TBitmappedFont
import scalaxy.loops._

class TextRenderer(WD : WindowingGraphicsData) extends WindowingRenderer(WD) {
	val layouter : TTextLayouter = PixelLayouter

	def effectiveFontFor(tw : TextDisplayWidget) = tw.font match {
		case Some(wrapper) => wrapper.font.apply(WD.textureBlock)
		case None => WD.defaultFont
	}

	def layout(tw : TextDisplayWidget) = {
		import tw._
		val area = Rectf(0.0f,0.0f,10000.0f,100000.0f)
		val font = effectiveFontFor(tw)
		layouter.layOutText(LayoutParameters(text, font, effectiveFontScale, area, 1.0f, effectiveFontScale/2, textAlignment))
	}

	override def render(widget: Widget, beforeChildren: Boolean): Seq[WQuad] = {
		widget match {
			case tw : TextDisplayWidget =>
				import tw._
				val effFont = effectiveFontFor(tw)
				TextRenderer.render(layouter, text, effFont, Rectf(0.0f,0.0f,10000.0f,100000.0f), effectiveFontScale, textAlignment)
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

object TextRenderer {
	def render(layouter : TTextLayouter, richText : RichText, effFont : TBitmappedFont, area : Rectf, effectiveFontScale : Float, textAlignment : Int) = {
		val res = layouter.layOutText(LayoutParameters(richText, effFont, effectiveFontScale, area, 1.0f, effectiveFontScale / 2, textAlignment))

		var si = 0
		var pi = 0
		var ret = Vector[WQuad]()
		var sections = richText.sections
		while (sections.nonEmpty) {
			if (si >= sections.head.symbolCount) {
				sections = sections.tail
				si = 0
			} else {
				val point = res.points(pi)
				val symbol = sections.head.symbolAtIndex(si)
				val x = point.x
				val y = point.y
				symbol match {
					case char : Char =>
						if (!char.isWhitespace) {
							val color = sections.head.colorAtIndex(si)
							val rawW = layouter.charWidth(char,effFont,effectiveFontScale)
							val rawH = layouter.charHeight(char,effFont,effectiveFontScale)
							val w = floorf( rawW + 0.0001f)
							val h = floorf( rawH + 0.0001f)
							//	TODO: bounds culling optimization
							//							if ( y < bounds.y + bounds.h && y + h > bounds.y ) {
							val tc = effFont.characterTexCoords(char)

							for (backgroundColor <- sections.head.backgroundColorAtIndex(si)) {
								ret :+= WQuad(Rectf(x,y,w,layouter.lineHeight(effFont, effectiveFontScale)), "default/blank.png", backgroundColor)
							}
							//							} else { break = true }
							ret :+= WQuad(Rectf(x,y,w,h),Image.Sentinel,color,0,WQuad.StandardRect, Some(tc))
						}
					case layers : List[ImageSectionLayer] =>
						val scale = sections.head.scaleAtIndex(si)
						for (layer <- layers) {
							val img = layer.image
							val w = img.width * scale
							val h = img.height * scale
							ret :+= WQuad(Rectf(x,y,w,h), img, layer.color, 0)
						}
				}

				pi += 1
				si += 1
			}
		}
		ret
	}
}


object PixelLayouter extends TextLayouter {
	override def charWidth(char: Char, font: TBitmappedFont, fontScale: Float): Float = {
		font.characterWidthPixels(char) / EngineCore.pixelScaleFactor * fontScale
	}

	override def charHeight(char: Char, font: TBitmappedFont, fontScale: Float): Float = {
		font.characterHeightPixels(char) / EngineCore.pixelScaleFactor * fontScale
	}

	override def lineHeight(font: TBitmappedFont, fontSize: Float): Float = font.lineHeightPixels / EngineCore.pixelScaleFactor * fontSize


	override def maxAscentPlusDescent(font: TBitmappedFont, fontSize: Float): Float = font.maxAscentPlusDescentPixels / EngineCore.pixelScaleFactor * fontSize


	override def descent(font: TBitmappedFont, fontSize: Float): Float = font.descentPixels / EngineCore.pixelScaleFactor * fontSize

	override def spaceSize(font : TBitmappedFont, fastFontSize: Float): Float = font.lineHeightPixels / EngineCore.pixelScaleFactor * 0.5f
}