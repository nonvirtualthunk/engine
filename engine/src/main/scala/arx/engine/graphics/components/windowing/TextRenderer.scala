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
import arx.graphics.helpers.{Color, ImageSectionLayer, TTextLayouter, TextLayouter}
import arx.graphics.helpers.TTextLayouter.LayoutParameters
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

	override def render(widget: Widget, beforeChildren: Boolean): Seq[WQuad] = {
		widget match {
			case tw : TextDisplayWidget =>
				import tw._
				val effFont = effectiveFontFor(tw)
				val res = layout(tw)

				var si = 0
				var pi = 0
				val richText = text.resolve() // note, by resolving twice this could change underneath us since layout
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