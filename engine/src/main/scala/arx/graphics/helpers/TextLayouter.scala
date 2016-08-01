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
import arx.core.vec.Cardinals
import arx.core.vec.ReadVec2f
import arx.core.vec.Vec2f
import arx.graphics.helpers.TTextLayouter.LayoutParameters
import arx.graphics.text.TBitmappedFont


class TextLayouter extends TTextLayouter {
	def layOutText (text : String, font : TBitmappedFont, fontSize : Float, area : Rectf, spacingMultiple : Float = 1.0f, minSpacing : Float = 0.0f, textAlignment : Int = Cardinals.Left ) : TextLayoutResult = {
		var x = 0.0f
		var y = 0.0f
		var maxX = 0.0f
		var maxY = 0.0f

		var pointList: List[Vec2f] = Nil

		def renderWord(word: String) {
			val width = stringWidth(word, font, fontSize)
			//If this is the only word on the line and it's longer than the line, no point skipping down
			if (x + width > area.w && x > 0.00001f) {
				y += lineSpacing(font,fontSize); x = 0.0f
				maxY = math.max(maxY,y)
			}
			var wordIndex = 0
			while ( wordIndex < word.length() ) {
				val c = word.charAt(wordIndex)
				val cw = charWidth(c, font, fontSize)
				//If we're mid-word when we hit then end, skip down
				if ( x + cw > area.w ) {
					x = 0.0f
					y += lineSpacing(font,fontSize); x = 0.0f
					maxY = math.max(maxY,y)
				}

				pointList ::= Vec2f(area.x + x,area.y + y)

				x += cw * spacingMultiple + minSpacing
				maxX = math.max(maxX,x)

				wordIndex += 1
			}
		}

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
						pointList ::= Vec2f(area.x + x,area.y + y)
						x += spaceSize(fontSize) * spacingMultiple
					case '\n' => {
						x = 0.0f; y += lineSpacing(font,fontSize)
						pointList ::= Vec2f(area.x + x,area.y + y)
					}
					case '\t' =>
						pointList ::= Vec2f(area.x + x,area.y + y)
						x += tabSize(fontSize) * spacingMultiple
					case _ => Noto.error("AAAAH")
				}
				maxX = math.max(maxX,x)
				i += 1
			}
		}

		val points = pointList.reverse
		if ( points.nonEmpty && textAlignment == Cardinals.Center ) {
			val minX = points.fmin(_.x - area.x)
			val maxX = points.fmax(_.x - area.x)
			val minY = points.fmin(_.y - area.y)
			val maxY = points.fmax(_.y - area.y)
			val ox = (area.width - (maxX - minX + charWidth(text.last,font,fontSize))) * 0.5f
			val oy = (area.height - (maxY - minY + charHeight(text.last,font,fontSize))) * 0.5f
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

			for ((p,t) <- points.zip(text)) {
				if (p.y != lastY) {
					adjustLine()
				}
				pointsOnLine :+= p
				maxXForLine = math.max(p.x + charWidth(t,font,fontSize),maxXForLine)
				lastY = p.y
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
	def lineSpacing ( font : TBitmappedFont , fontSize : Float ) = font.maxCharacterDimensions.y * fontSize * 0.75f
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

case class TextLayoutResult ( points : List[ReadVec2f] , dimensions : ReadVec2f )