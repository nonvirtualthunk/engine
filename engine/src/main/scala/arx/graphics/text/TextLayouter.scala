package arx.graphics.text

import java.awt.Font
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

import arx.application.Noto
import arx.core.math.Rectf
import arx.core.vec.{ReadVec2f, Vec2f}
import arx.graphics.helpers.{ImageSection, RichText, TextSection}
import arx.Prelude._
import arx.engine.EngineCore
import arx.graphics.text.VerticalTextAlignment.{Bottom, Top}
import scalaxy.loops._

import scala.collection.mutable.ArrayBuffer

class TextLayouter protected[text](val font : TBitmappedFont, val scale : Float) {
	protected[text] val fm = font.fontMetrics
	protected[text] var spacingMultiple = 1.0f
	protected[text] var minSpacing = 0.0f

	def withSpacingMultiple(x : Float): TextLayouter = {
		spacingMultiple = x
		this
	}
	def withMinSpacing(x : Float): TextLayouter = {
		minSpacing = x
		this
	}

	def lineHeight: Float = fm.lineHeight * scale
	def spaceSize: Float = fm.spaceSize * scale
	def tabSize: Float = spaceSize * 3
	def descent: Float = fm.maxDescent * scale

	def layoutText (text : RichText, area : Rectf, horizontalAlignment : HorizontalTextAlignment = HorizontalTextAlignment.Left, verticalAlignment : VerticalTextAlignment = Bottom) : TextLayoutResult = {
		val fm = font.fontMetrics

		var x = 0.0f
		var y = 0.0f
		var maxX = 0.0f
		var maxY = 0.0f

		var maxSpacingThisLine = 0.0f

		var rects: Vector[Rectf] = Vector()
		val lineRectStartIndices : ArrayBuffer[Int] = ArrayBuffer(0)

		def jumpToNextLine(): Unit = {
			y += lineHeight.max(maxSpacingThisLine)
			x = 0.0f
			maxY = math.max(maxY, y)
			maxSpacingThisLine = 0.0f
			lineRectStartIndices.append(rects.length)
		}

		def jumpIfNecessary(w : Float): Unit = {
			if (x + w >= area.w && x > area.x + 0.00001f) {
				jumpToNextLine()
			}
		}

		def addRect(rect : Rectf): Unit = {
			rects :+= rect
		}

		def renderWord(word: String) {
			if (word.nonEmpty) {
				val frc = new FontRenderContext(AffineTransform.getScaleInstance(scale * EngineCore.pixelScaleFactor, scale * EngineCore.pixelScaleFactor), !font.pixelFont, !font.pixelFont)

				val glyphVector = font.font.layoutGlyphVector(frc, word.toCharArray, 0, word.length, Font.LAYOUT_LEFT_TO_RIGHT)
				val wordWidth = glyphVector.getGlyphPixelBounds(glyphVector.getNumGlyphs - 1, frc, 0, 0).x.toFloat +
					font.characterWidthPixels(word(glyphVector.getNumGlyphs - 1)) * scale

				jumpIfNecessary(wordWidth)

				for (i <- 0 until glyphVector.getNumGlyphs) {
					val glyphRect = glyphVector.getGlyphPixelBounds(i, frc, 0, 0)
					if (x == area.x && glyphRect.getX < 0.0) {
						x += -glyphRect.getX.toFloat
					}
					addRect(Rectf(area.x + x + glyphRect.getX.toFloat, area.y + y, font.characterWidthPixels(word(i)) * scale, font.characterHeightPixels(word(i)) * scale))
				}

				x += wordWidth
				maxX = maxX.max(x)
			}
		}

		for (section <- text.sections) {
			val pre = rects.size
			section match {
				case TextSection(text,_, _) =>
					val words = text.split(TextLayouter.whitespaceArray)
					var i = 0

					def advanceThroughWitespace(): Unit = {
						while (i < text.length && TextLayouter.whitespaceSet.contains(text(i))) {
							text(i) match {
								case ' ' =>
									val sw = spaceSize * spacingMultiple
									addRect(Rectf(area.x + x,area.y + y, sw, lineHeight))
									x += sw
								case '\n' => {
									jumpToNextLine()
									addRect(Rectf(area.x + x,area.y + y, 0.0f, lineHeight))
								}
								case '\t' =>
									val tw = tabSize * spacingMultiple
									addRect(Rectf(area.x + x,area.y + y, tw, lineHeight))
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
				case ImageSection(layers, imgScale) =>
					val width = layers.imax(l => l.image.width) * imgScale
					val height = layers.imax(l => l.image.height) * imgScale
					//If this is the only thing on the line and it's longer than the line, no point skipping down, otherwise
					jumpIfNecessary(width)
					maxSpacingThisLine = height // ensure that we don't collide with this image on the next line

					addRect(Rectf(area.x + x,area.y + y + (lineHeight - height) / 2, width, height))

					x += width + minSpacing
					maxX = math.max(maxX,x)
			}
			if (pre + section.symbolCount != rects.size) {
				println("wrong")
			}
		}

		if (horizontalAlignment != HorizontalTextAlignment.Left && rects.nonEmpty) {
			for (lineNumber <- 0 until lineRectStartIndices.size optimized) {
				val lineStart = lineRectStartIndices(lineNumber)
				val lineEnd = if (lineNumber < lineRectStartIndices.size - 1) {
					lineRectStartIndices(lineNumber + 1)
				} else {
					rects.size
				}

				val minX = rects(lineStart).minX
				val maxX = rects(lineEnd-1).maxX
				val lineWidth = maxX - minX
				val adjustedStartX = area.x + (if (horizontalAlignment == HorizontalTextAlignment.Centered) {
					(area.w - lineWidth) / 2.0f
				} else if (horizontalAlignment == HorizontalTextAlignment.Right) {
					area.w - lineWidth
				} else { Noto.warn(s"invalid horizontal text alignment $horizontalAlignment"); minX })

				val delta = adjustedStartX - minX
				for (i <- lineStart until lineEnd optimized) {
					rects(i).x += delta
				}
			}
		}

		if (verticalAlignment != VerticalTextAlignment.Bottom && rects.nonEmpty) {
			val minY = rects.head.minY
			val maxY = rects.last.maxY
			val totalHeight = (maxY - minY).max(lineHeight)
			val adjustedStartY = area.y + (if (verticalAlignment == VerticalTextAlignment.Centered) {
				(area.h - totalHeight) / 2.0f
			} else if (verticalAlignment == VerticalTextAlignment.Top) {
				area.h - totalHeight
			} else { Noto.warn(s"invalid vertical text alignment $verticalAlignment"); minY })

			val delta = adjustedStartY - minY
			rects.foreach(_.y += delta)
		}

//		if ( rects.nonEmpty && horizontalAlignment == HorizontalTextAlignment.Centered) {
//			val lastSymbolWidth = widths.last
//			val minX = rects.fmin(_.x - area.x)
//			val maxX = rects.fmax(_.x - area.x)
//			val minY = rects.fmin(_.y - area.y)
//			val maxY = rects.fmax(_.y - area.y)
//			val ox = (area.width - (maxX - minX + lastSymbolWidth)) * 0.5f
//			val oy = (area.height - (maxY - minY + lastSymbolWidth)) * 0.5f
//			for (p <- rects) { p.x += ox; p.y += oy }
//		} else if ( rects.nonEmpty && horizontalAlignment == HorizontalTextAlignment.Right) {
//			var maxXForLine = 0.0f
//			var lastY = rects.head.y
//			var rectsOnLine = Vector[Rectf]()
//			def adjustLine (): Unit = {
//				val offset = (area.x + area.w) - maxXForLine
//				for (lp <- rectsOnLine) {
//					lp.x += offset
//				}
//				rectsOnLine = Vector()
//				maxXForLine = 0.0f
//			}
//
//			var i = 0
//			while (i < rects.size) {
//				val p = rects(i)
//				val w = widths(i)
//				if (p.y != lastY) {
//					adjustLine()
//				}
//				rectsOnLine :+= p
//				maxXForLine = math.max(p.x + w,maxXForLine)
//				lastY = p.y
//				i += 1
//			}
//			adjustLine()
//		}

		val lastLineHeight = lineHeight + descent + 4
		TextLayoutResult(rects,Vec2f(maxX,maxY + lastLineHeight.max(maxSpacingThisLine)), font, lineHeight)
	}
}


case class TextLayoutResult(glyphRects: Vector[Rectf], dimensions: ReadVec2f, font: TBitmappedFont, lineHeight: Float)

object TextLayouter {
	val whitespaceArray: Array[Char] = Array[Char](' ', '\n', '\t')
	val whitespaceSet: Set[Char] = whitespaceArray.toSet


	def apply(baseFont : TBitmappedFont, fontScale : Float) = {
		if (baseFont.pixelFont) {
			new TextLayouter(baseFont, fontScale)
		} else {
			new TextLayouter(baseFont.deriveWithPtSize((baseFont.font.getSize * fontScale).toInt), 1.0f)
		}
	}
}


sealed trait HorizontalTextAlignment
object HorizontalTextAlignment {
	case object Left extends HorizontalTextAlignment
	case object Right extends HorizontalTextAlignment
	case object Centered extends HorizontalTextAlignment
}

sealed trait VerticalTextAlignment
object VerticalTextAlignment {
	case object Top extends VerticalTextAlignment
	case object Bottom extends VerticalTextAlignment
	case object Centered extends VerticalTextAlignment
}