package arx.graphics.helpers

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/1/12
 * Time: 8:24 AM
 * Created by nonvirtualthunk
 */

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
}
case class TextSection(text : String, color : ReadVec4f = Color.Black) extends RichTextSection {
	override def symbolAtIndex(i: Int): Any = text(i)
	override def symbolCount: Int = text.length
	override def colorAtIndex(i: Int): ReadVec4f = color
}
case class ImageSection(image : TToImage, scale : Float, color : ReadVec4f = Color.Black) extends RichTextSection {
	override def symbolAtIndex(i: Int): Any = i match {
		case 0 => image.image
		case _ => Noto.severeError("Out of bounds access to image rich-text section"); '~'
	}
	override def symbolCount: Int = 1
	override def colorAtIndex(i: Int): ReadVec4f = color
}

case class RichText (sections : List[RichTextSection]) {
	def symbolCount = sections.isum(s => s.symbolCount)
	protected def getFromIndex[T](target : Int, func : (TextSection, Int) => T) = {
		var tmpS = sections
		var i = target
		while (tmpS.nonEmpty && i >= tmpS.head.symbolCount) {
			i -= tmpS.head.symbolCount
			tmpS = tmpS.tail
		}
		if (tmpS.nonEmpty) {
			func(tmpS(i))
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


class TextLayouter extends TTextLayouter {
	def layOutText (text : String, font : TBitmappedFont, fontSize : Float, area : Rectf, spacingMultiple : Float = 1.0f, minSpacing : Float = 0.0f, textAlignment : Int = Cardinals.Left ) : TextLayoutResult = {
		layOutText(RichText(TextSection(text) :: Nil), font, fontSize, area, spacingMultiple, minSpacing, textAlignment)
	}

	def layOutText (richText : RichText, font : TBitmappedFont, fontSize : Float, area : Rectf, spacingMultiple : Float = 1.0f, minSpacing : Float = 0.0f, textAlignment : Int = Cardinals.Left ) : TextLayoutResult = {
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
			val width = stringWidth(word, font, fontSize)
			//If this is the only word on the line and it's longer than the line, no point skipping down
			if (x + width > area.w && x > 0.00001f) {
				jumpToNextLine()
			}
			var wordIndex = 0
			while ( wordIndex < word.length() ) {
				val c = word.charAt(wordIndex)
				val cw = charWidth(c, font, fontSize)
				//If we're mid-word when we hit then end, skip down
				if ( x + cw > area.w ) {
					jumpToNextLine()
				}

				points :+= Vec2f(area.x + x,area.y + y)
				widths :+= cw

				x += cw * spacingMultiple + minSpacing
				maxX = math.max(maxX,x)

				wordIndex += 1
			}
		}

		for (section <- richText.sections) {
			section match {
				case TextSection(text,_) =>
					val words = text.split(TextLayouter.whitespaceArray)
					var i = 0
					for (word <- words) {
						if (!word.isEmpty) {
							renderWord(word)
							i += word.length
						}
						while (i < text.length && TextLayouter.whitespaceSet.contains(text(i))) {
							text(i) match {
								case ' ' =>
									val sw = spaceSize(fontSize) * spacingMultiple
									points :+= Vec2f(area.x + x,area.y + y)
									widths :+= sw
									x += sw
								case '\n' => {
									jumpToNextLine()
									points :+= Vec2f(area.x + x,area.y + y)
									widths :+= 0.0f
								}
								case '\t' =>
									val tw = tabSize(fontSize) * spacingMultiple
									points :+= Vec2f(area.x + x,area.y + y)
									widths :+= tw
									x += tw
								case _ => Noto.error("AAAAH")
							}
							maxX = math.max(maxX,x)
							i += 1
						}
					}
				case ImageSection(image, scale, _) =>
					val width = image.width * scale
					//If this is the only thing on the line and it's longer than the line, no point skipping down, otherwise
					if (x + width > area.w && x > 0.00001f) {
						jumpToNextLine()
					}
					maxSpacingThisLine = image.height * scale // ensure that we don't collide with this image on the next line

					points :+= Vec2f(area.x + x,area.y + y)
					widths :+= width

					x += width + minSpacing
					maxX = math.max(maxX,x)
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

		TextLayoutResult(points,Vec2f(maxX,maxY + lineHeight(font,fontSize)))
	}
	def textDimensions ( text : String , font : TBitmappedFont , fontSize : Float , area : Rectf , spacingMultiple : Float = 1.0f , minSpacing : Float = 0.0f ) : Vec2f = {
		layOutText(text,font,fontSize,area,spacingMultiple,minSpacing).dimensions
	}

	def stringWidth(str: String, font: TBitmappedFont, fastFontSize : Float ): Float = {
		var sum = 0.0f
		var i = 0;while ( i < str.length ) {
			sum += charWidth(str.charAt(i), font, fastFontSize)
		i += 1}
		sum
	}

	def charWidth(char: Char, font: TBitmappedFont, fastFontSize : Float ): Float = {
		if ( char == '\n' ) { 0.0f }
		else if ( char == '\t' ) { tabSize(fastFontSize) }
		else if ( char == ' ' ) { spaceSize(fastFontSize) }
		else {
			font.characterWidth(char) * fastFontSize
		}
	}

	def charHeight(char: Char, font: TBitmappedFont, fastFontSize : Float ): Float = {
		font.characterHeight(char) * fastFontSize
	}

	def lineHeight ( font : TBitmappedFont , fontSize : Float ) = font.maxCharacterDimensions.y * fontSize
	def lineSpacing ( font : TBitmappedFont , fontSize : Float ) = lineHeight(font, fontSize) * 0.75f
	def spaceSize(fastFontSize : Float ): Float = fastFontSize * 0.3f

	def tabSize(fastFontSize : Float): Float = spaceSize(fastFontSize) * 3.0f
}

object TextLayouter {
	val whitespaceArray = Array[Char](' ', '\n', '\t')
	val whitespaceSet = whitespaceArray.toSet
}

trait TTextLayouter {
	def layOutText ( text : String , font : TBitmappedFont , fontSize : Float , area : Rectf , spacingMultiple : Float = 1.0f, minSpacing : Float = 0.0f,textAlignment :Int = Cardinals.Left) : TextLayoutResult
	def layOutText ( params : LayoutParameters ) : TextLayoutResult = {
		layOutText(params.text,params.font,params.fontSize,params.area,params.spacingMultiple,params.minSpacing,params.textAlignment)
	}
	def textDimensions ( text : String , font : TBitmappedFont , fontSize : Float , area : Rectf , spacingMultiple : Float = 1.0f, minSpacing : Float = 0.0f) : ReadVec2f
	def textDimensions ( params : LayoutParameters ) : ReadVec2f = {
		textDimensions(params.text,params.font,params.fontSize,params.area,params.spacingMultiple,params.minSpacing)
	}
	def charWidth(char: Char, font: TBitmappedFont, fastFontSize : Float ): Float
	def charHeight(char: Char, font: TBitmappedFont, fastFontSize : Float ): Float
}

object TTextLayouter {
	case class LayoutParameters ( text : String , font : TBitmappedFont , fontSize : Float , area : Rectf , spacingMultiple : Float, minSpacing : Float, textAlignment : Int )
}

case class TextLayoutResult ( points : Vector[ReadVec2f] , dimensions : ReadVec2f )