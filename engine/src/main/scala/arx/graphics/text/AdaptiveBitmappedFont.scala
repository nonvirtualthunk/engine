package arx.graphics.text

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/15/13
 * Time: 8:49 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.core.richer.IntRange
import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec2i
import arx.core.vec.Vec2f
import arx.core.vec.Vec2i
import arx.graphics.Image
import arx.graphics.TextureBlock
import org.lwjgl.opengl.GL11

import scala.collection.mutable
import scala.collection.JavaConversions._

class AdaptiveBitmappedFont(var glyphSources: List[GlyphSource], backingTextureBlock: TextureBlock = TextureBlock.Sentinel, val basePointSize: Float = 60.0f, val pixelFont: Boolean = false, drop: Int = 0) extends TBitmappedFont {
	def this() { this(Nil, TextureBlock.Sentinel) }

	var textureBlock = glyphSources match {
		case Nil => null
		case l => if (backingTextureBlock.isSentinel) {new TextureBlock(2048, 2048) }
		else {backingTextureBlock }
	}

	val asciiRange: IntRange = 32 -> 127

	var asciiTexCoords = Array.ofDim[ReadVec2f](asciiRange.upper, 4)
	var unicodeTexCoords = new mutable.HashMap[Char, Array[ReadVec2f]]

	var asciiCharacterWidths = Array.ofDim[Float](asciiRange.upper)
	var asciiCharacterPixelWidths = Array.ofDim[Int](asciiRange.upper)
	var asciiCharacterHeights = Array.ofDim[Float](asciiRange.upper)
	var asciiCharacterPixelHeights = Array.ofDim[Int](asciiRange.upper)
	var unicodeCharacterDimensions = new mutable.HashMap[Char, ReadVec2f]
	var unicodeCharacterPixelDimensions = new mutable.HashMap[Char, ReadVec2i]

	var _maxCharacterDimensions = Vec2f(0.0f, 0.0f)
	var _maxCharacterDimensionsPixels = Vec2i(0,0)

	var needsSolidification = false

	if (glyphSources.nonEmpty) {init() }

	def init() {
		if (backingTextureBlock.isSentinel) {
			textureBlock.minFilter = GL11.GL_LINEAR_MIPMAP_LINEAR
			textureBlock.magFilter = GL11.GL_LINEAR
		}
		//		textureBlock.minFilter = GL11.GL_NEAREST
		//		textureBlock.magFilter = GL11.GL_LINEAR
		println("Creating adaptive bitmapped font")

		var ch = 0
		while (ch < asciiRange.upper) {
			if (ch >= asciiRange.lower) {
				val img = glyphSources.find(_.canProvideGlyphFor(ch.toChar)) match {
					case Some(src) => src.glyphFor(ch.toChar)
					case None => Image.Sentinel
				}

				if (img.width > 0 && img.height > 0) {
					//					val charWidth = img.width.toFloat / fontHelper.pixelSize
					//					val charHeight = img.height.toFloat / fontHelper.pixelSize
					val charWidth = img.width.toFloat / img.height.toFloat
					val charHeight = 1.0f

					val tc = textureBlock.getOrElseUpdate(img)
					asciiTexCoords(ch) = tc
					asciiCharacterWidths(ch) = charWidth
					asciiCharacterHeights(ch) = charHeight
					asciiCharacterPixelWidths(ch) = img.width
					asciiCharacterPixelHeights(ch) = img.height
					_maxCharacterDimensions.x = math.max(_maxCharacterDimensions.x, charWidth)
					_maxCharacterDimensions.y = math.max(_maxCharacterDimensions.y, charHeight)
					_maxCharacterDimensionsPixels.x = math.max(_maxCharacterDimensionsPixels.x, img.width)
					_maxCharacterDimensionsPixels.y = math.max(_maxCharacterDimensionsPixels.y, img.height)
				}
			} else {
				asciiTexCoords(ch) = Array.fill(4)(Vec2f.Zero)
				asciiCharacterWidths(ch) = 0.001f
				asciiCharacterHeights(ch) = 0.001f
			}
			ch += 1
		}

		val img = arx.graphics.Image.withDimensions(textureBlock.width, textureBlock.height)
		import arx.core.vec._

		import scalaxy.loops._
		var min = Vec2i(img.width, img.height)
		var max = Vec2i(0, 0)
		for ((i, rdata) <- textureBlock.subTextures) {
			val xs = rdata.location.x
			val ys = rdata.location.y
			min = min.min(Vec2i(xs, ys))
			max = max.max(Vec2i(xs + i.width, ys + i.height))
			for (x <- xs until xs + i.width optimized; y <- ys until ys + i.height optimized) {
				for (q <- 0 until 4 optimized) {
					img(x, y, q) = textureBlock(x, y, q)
				}
			}
		}

		val nImg = arx.graphics.Image.withDimensions(max.x, max.y)
		nImg.setPixelsFromFunc((x, y, q) => {
			img(x, y, q)
		})
		arx.graphics.Image.save(nImg, "save/caches/font_texture.png")

		println("After font creation, percent available : " + (textureBlock.availableSpace / (textureBlock.width * textureBlock.height).toFloat))
	}

	def characterTexCoords(c: Char): Array[ReadVec2f] = {
		if (asciiRange.contains(c)) {
			asciiTexCoords(c)
		} else {
			ensureUnicodeCharAdded(c)
			unicodeTexCoords(c)
		}
	}

	def characterWidth(c: Char) = {
		if (asciiRange.contains(c)) {
			asciiCharacterWidths(c)
		} else {
			ensureUnicodeCharAdded(c)
			unicodeCharacterDimensions(c).x
		}
	}

	def characterHeight(c: Char) = {
		if (asciiRange.contains(c)) {
			asciiCharacterHeights(c)
		} else {
			ensureUnicodeCharAdded(c)
			unicodeCharacterDimensions(c).y
		}
	}

	override def characterWidthPixels(c: Char): Int = {
		if (asciiRange.contains(c)) {
			asciiCharacterPixelWidths(c)
		} else {
			ensureUnicodeCharAdded(c)
			unicodeCharacterPixelDimensions(c).x
		}
	}

	override def characterHeightPixels(c: Char): Int = {
		if (asciiRange.contains(c)) {
			asciiCharacterPixelHeights(c)
		} else {
			ensureUnicodeCharAdded(c)
			unicodeCharacterPixelDimensions(c).y
		}
	}

	def bind(i: Int) {
		textureBlock.bind(i)
	}

	def maxCharacterDimensions = _maxCharacterDimensions
	override def maxCharacterDimensionsPixels: ReadVec2i = _maxCharacterDimensionsPixels

	def ensureUnicodeCharAdded(c: Char) {
		if (!unicodeTexCoords.contains(c)) {
			val img = glyphSources.find(_.canProvideGlyphFor(c.toChar)) match {
				case Some(src) => src.glyphFor(c.toChar)
				case None => Image.Sentinel
			}

			val charWidth = img.width.toFloat / img.height.toFloat
			val charHeight = 1.0f

			//			val charWidth = img.width.toFloat / fontHelper.pixelSize
			//			val charHeight = img.height.toFloat / fontHelper.pixelSize
			val tc = textureBlock.getOrElseUpdate(img)
			//
			_maxCharacterDimensions.x = math.max(_maxCharacterDimensions.x, charWidth)
			_maxCharacterDimensions.y = math.max(_maxCharacterDimensions.y, charHeight)
			_maxCharacterDimensionsPixels.x = math.max(_maxCharacterDimensionsPixels.x, img.width)
			_maxCharacterDimensionsPixels.y = math.max(_maxCharacterDimensionsPixels.y, img.height)

			unicodeCharacterDimensions(c) = Vec2f(charWidth, charHeight)
			unicodeCharacterPixelDimensions(c) = Vec2i(img.width, img.height)
			unicodeTexCoords(c) = tc
		}
	}
}

object AdaptiveBitmappedFont {
}
