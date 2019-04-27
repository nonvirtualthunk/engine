package arx.graphics.helpers

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/1/12
 * Time: 8:24 AM
 * Created by nonvirtualthunk
 */

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

import arx.Prelude._
import arx.application.Noto
import arx.core.math.Rectf
import arx.core.vec.{Cardinals, ReadVec2f, ReadVec4f, Vec2f}
import arx.graphics.TToImage
import arx.graphics.helpers.TTextLayouter.LayoutParameters
import arx.graphics.text.TBitmappedFont


sealed abstract class RichTextSection {
	def symbolAtIndex(i : Int) : Any
	def symbolCount : Int
	def colorAtIndex(i : Int) : ReadVec4f
	def backgroundColorAtIndex(i : Int) : Option[ReadVec4f] = None
	def scaleAtIndex(i : Int) : Float
	def merge (s : RichTextSection) : Option[RichTextSection] = None
}
case class TextSection(text : String, color : ReadVec4f = Color.Black, backgroundColor : Option[ReadVec4f] = None) extends RichTextSection {
	override def symbolAtIndex(i: Int): Any = text(i)
	override def symbolCount: Int = text.length
	override def colorAtIndex(i: Int): ReadVec4f = color
	override def scaleAtIndex(i : Int) : Float = 1.0f
	override def backgroundColorAtIndex(i : Int) : Option[ReadVec4f] = backgroundColor
	override def merge (s : RichTextSection) : Option[RichTextSection] = s match {
		case ts : TextSection if ts.color == color => Some(TextSection(text + ts.text, color))
		case _ => None
	}
}
case class ImageSectionLayer(image : TToImage, color : ReadVec4f = Color.White)
case class ImageSection(layers : List[ImageSectionLayer], scale : Float) extends RichTextSection {
	override def symbolAtIndex(i: Int): Any = i match {
		case 0 => layers
		case _ => Noto.severeError("Out of bounds access to image rich-text section"); '~'
	}
	override def symbolCount: Int = 1
	override def colorAtIndex(i: Int): ReadVec4f = layers.head.color
	override def scaleAtIndex(i : Int) : Float = scale
}
object ImageSection {
	def apply(image : TToImage, scale : Float, color : ReadVec4f) : ImageSection = ImageSection(ImageSectionLayer(image, color) :: Nil, scale)
}

case class RichText (sections : Seq[RichTextSection]) {
	def symbolCount = sections.isum(s => s.symbolCount)
	protected def getFromIndex[T](target : Int, func : (RichTextSection, Int) => T) = {
		var tmpS = sections
		var i = target
		while (tmpS.nonEmpty && i >= tmpS.head.symbolCount) {
			i -= tmpS.head.symbolCount
			tmpS = tmpS.tail
		}
		if (tmpS.nonEmpty) {
			func(tmpS(i), i)
		} else {
			throw new IndexOutOfBoundsException("Attempted to access past the end of a rich text")
		}
	}
	def symbolAtIndex(target : Int) = {
		getFromIndex(target, (s, i) => s.symbolAtIndex(i))
	}
	def colorAtIndex(target : Int) = {
		getFromIndex(target, (s,i) => s.colorAtIndex(i))
	}
}
object RichText {
	implicit def apply (str : String) : RichText = RichText(List(TextSection(str)))
	val Empty = RichText(Nil)
}


class TextLayouter extends TTextLayouter {

	def layOutText(richText : RichText, font : TBitmappedFont, fontSize : Float, area : Rectf, spacingMultiple : Float = 1.0f, minSpacing : Float = 0.0f, textAlignment : Int = Cardinals.Left ) : TextLayoutResult = {
		var x = 0.0f
		var y = 0.0f
		var maxX = 0.0f
		var maxY = 0.0f

		var maxSpacingThisLine = 0.0f

		var points: Vector[Vec2f] = Vector()
		var widths : Vector[Float] = Vector()

		def jumpToNextLine() = {
			y += lineSpacing(font, fontSize).max(maxSpacingThisLine)
			x = 0.0f
			maxY = math.max(maxY, y)
			maxSpacingThisLine = 0.0f
		}

		def renderWord(word: String) {
			val frc = new FontRenderContext(AffineTransform.getScaleInstance(fontSize, fontSize), !font.pixelFont, !font.pixelFont)

			val glyphVector = font.font.layoutGlyphVector(frc, word.toCharArray, 0, word.length, Font.LAYOUT_LEFT_TO_RIGHT)

			for (i <- 0 until glyphVector.getNumGlyphs) {
				val glyphRect = glyphVector.getGlyphPixelBounds(i, frc, 0, 0)
				if (x == 0.0f && glyphRect.getX < 0.0) {
					x += -glyphRect.getX.toFloat
				}
				points :+= Vec2f(x + glyphRect.getX.toFloat, y)
				val width = glyphRect.getWidth.toFloat
				widths :+= width
			}
			x += glyphVector.getGlyphPixelBounds(glyphVector.getNumGlyphs - 1, frc, 0, 0).getMaxX.toFloat
			maxX = maxX.max(x)
		}

		for (section <- richText.sections) {
			val pre = points.size
			section match {
				case TextSection(text,_, _) =>
					val words = text.split(TextLayouter.whitespaceArray)
					var i = 0

					def advanceThroughWitespace(): Unit = {
						while (i < text.length && TextLayouter.whitespaceSet.contains(text(i))) {
							text(i) match {
								case ' ' =>
									val sw = spaceSize(font, fontSize) * spacingMultiple
									points :+= Vec2f(area.x + x,area.y + y)
									widths :+= sw
									x += sw
								case '\n' => {
									jumpToNextLine()
									points :+= Vec2f(area.x + x,area.y + y)
									widths :+= 0.0f
								}
								case '\t' =>
									val tw = tabSize(font, fontSize) * spacingMultiple
									points :+= Vec2f(area.x + x,area.y + y)
									widths :+= tw
									x += tw
								case _ => Noto.error("AAAAH")
							}
							maxX = math.max(maxX,x)
							i += 1
						}
					}


					advanceThroughWitespace()
					for (word <- words) {
						if (!word.isEmpty) {
							renderWord(word)
							i += word.length
						}
						advanceThroughWitespace()
					}
				case ImageSection(layers, scale) =>
					val width = layers.imax(l => l.image.width) * scale
					val height = layers.imax(l => l.image.height) * scale
					//If this is the only thing on the line and it's longer than the line, no point skipping down, otherwise
					if (x + width > area.w && x > 0.00001f) {
						jumpToNextLine()
					}
					maxSpacingThisLine = height // ensure that we don't collide with this image on the next line

					points :+= Vec2f(area.x + x,area.y + y + (lineHeight(font, fontSize) - height) / 2)
					widths :+= width

					x += width + minSpacing
					maxX = math.max(maxX,x)
			}
			if (pre + section.symbolCount != points.size) {
				println("wrong")
			}
		}


		if ( points.nonEmpty && textAlignment == Cardinals.Center ) {
			val lastSymbolWidth = widths.last
			val minX = points.fmin(_.x - area.x)
			val maxX = points.fmax(_.x - area.x)
			val minY = points.fmin(_.y - area.y)
			val maxY = points.fmax(_.y - area.y)
			val ox = (area.width - (maxX - minX + lastSymbolWidth)) * 0.5f
			val oy = (area.height - (maxY - minY + lastSymbolWidth)) * 0.5f
			for (p <- points) { p.x += ox; p.y += oy }
		} else if ( points.nonEmpty && textAlignment == Cardinals.Right ) {
			var maxXForLine = 0.0f
			var lastY = points.head.y
			var pointsOnLine = Vector[Vec2f]()
			def adjustLine (): Unit = {
				val offset = (area.x + area.w) - maxXForLine
				for (lp <- pointsOnLine) {
					lp.x += offset
				}
				pointsOnLine = Vector()
				maxXForLine = 0.0f
			}

			var i = 0
			while (i < points.size) {
				val p = points(i)
				val w = widths(i)
				if (p.y != lastY) {
					adjustLine()
				}
				pointsOnLine :+= p
				maxXForLine = math.max(p.x + w,maxXForLine)
				lastY = p.y
				i += 1
			}
			adjustLine()
		}

		val lastLineHeight = lineHeight(font, fontSize) + descent(font, fontSize)
		TextLayoutResult(points,Vec2f(maxX,maxY + lastLineHeight.max(maxSpacingThisLine)))
	}
	def textDimensions ( text : RichText , font : TBitmappedFont , fontSize : Float , area : Rectf , spacingMultiple : Float = 1.0f , minSpacing : Float = 0.0f ) : ReadVec2f = {
		layOutText(text,font,fontSize,area,spacingMultiple,minSpacing).dimensions
	}

	def stringWidth(str: String, font: TBitmappedFont, fastFontSize : Float ): Float = {
		var sum = 0.0f
		var i = 0;while ( i < str.length ) {
			sum += charWidth(str.charAt(i), font, fastFontSize)
		i += 1}
		sum
	}

	def maxAscentPlusDescent(font: TBitmappedFont, fontSize: Float) : Float = {
		font.maxAscentPlusDescentProportional * fontSize
	}

	def descent(font : TBitmappedFont, fontSize : Float) : Float = {
		???
	}

	def charWidth(char: Char, font: TBitmappedFont, fastFontSize : Float ): Float = {
		if ( char == '\n' ) { 0.0f }
		else if ( char == '\t' ) { tabSize(font, fastFontSize) }
		else if ( char == ' ' ) { spaceSize(font, fastFontSize) }
		else {
			font.characterWidthProportional(char) * fastFontSize
		}
	}

	def charHeight(char: Char, font: TBitmappedFont, fastFontSize : Float ): Float = {
		font.characterHeightProportional(char) * fastFontSize
	}

	def lineHeight ( font : TBitmappedFont , fontSize : Float ) = font.lineHeightProportional * fontSize
	def lineSpacing ( font : TBitmappedFont , fontSize : Float ) = lineHeight(font, fontSize)
	def spaceSize(font : TBitmappedFont, fontSize : Float ): Float = fontSize * 0.3f

	def tabSize(font : TBitmappedFont, fontSize : Float): Float = spaceSize(font, fontSize) * 3.0f
}

object TextLayouter {
	val whitespaceArray = Array[Char](' ', '\n', '\t')
	val whitespaceSet = whitespaceArray.toSet
}

trait TTextLayouter {
	def layOutText ( text : RichText , font : TBitmappedFont , fontSize : Float , area : Rectf , spacingMultiple : Float = 1.0f, minSpacing : Float = 0.0f,textAlignment :Int = Cardinals.Left) : TextLayoutResult
	def layOutText ( params : LayoutParameters ) : TextLayoutResult = {
		layOutText(params.text,params.font,params.fontSize,params.area,params.spacingMultiple,params.minSpacing,params.textAlignment)
	}



	def textDimensions ( text : RichText , font : TBitmappedFont , fontSize : Float , area : Rectf , spacingMultiple : Float = 1.0f, minSpacing : Float = 0.0f) : ReadVec2f
	def textDimensions ( params : LayoutParameters ) : ReadVec2f = {
		textDimensions(params.text,params.font,params.fontSize,params.area,params.spacingMultiple,params.minSpacing)
	}
	def charWidth(char: Char, font: TBitmappedFont, fastFontSize : Float ): Float
	def charHeight(char: Char, font: TBitmappedFont, fastFontSize : Float ): Float

	def lineHeight(font : TBitmappedFont, fastFontSize : Float) : Float
}

object TTextLayouter {
	case class LayoutParameters ( text : RichText , font : TBitmappedFont , fontSize : Float , area : Rectf , spacingMultiple : Float, minSpacing : Float, textAlignment : Int )
}

case class TextLayoutResult ( points : Vector[ReadVec2f] , dimensions : ReadVec2f )