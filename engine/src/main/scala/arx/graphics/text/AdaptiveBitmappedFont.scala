package arx.graphics.text

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/15/13
 * Time: 8:49 AM
 * Created by nonvirtualthunk
 */

import java.awt.Font

import arx.Prelude._
import arx.application.Noto
import arx.core.metrics.Metrics
import arx.core.richer.IntRange
import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec2i
import arx.core.vec.Vec2f
import arx.core.vec.Vec2i
import arx.engine.EngineCore
import arx.graphics.Image
import arx.graphics.TextureBlock
import org.cliffc.high_scale_lib.NonBlockingHashMap
import org.lwjgl.opengl.GL11
import overlock.atomicmap.AtomicMap

import scala.collection.mutable
import scala.collection.JavaConversions._

class AdaptiveBitmappedFont(val font: Font, var additionalGlyphSources: List[GlyphSource], backingTextureBlock: TextureBlock = TextureBlock.Sentinel, val pixelFont: Boolean = false) extends TBitmappedFont {
	def this() { this(new Font("SansSerif",Font.PLAIN, 32), Nil, TextureBlock.Sentinel) }


	var derivedBySize = AtomicMap.atomicNBHM[Int, AdaptiveBitmappedFont]
	val fontHelper = new FontHelper(font, pixelFont)
	val glyphSources : List[GlyphSource] = additionalGlyphSources ::: new AWTFontGlyphSource(fontHelper) :: Nil

	val textureBlock: TextureBlock = glyphSources match {
		case Nil => null
		case l => if (backingTextureBlock.isSentinel) {new TextureBlock(2048, 2048) }
		else {backingTextureBlock }
	}

	protected[text] val asciiRange: IntRange = 32 -> 127

	protected[text] var asciiTexCoords: Array[Array[ReadVec2f]] = Array.ofDim[ReadVec2f](asciiRange.upper, 4)
	protected[text] var unicodeTexCoords = new mutable.HashMap[Char, Array[ReadVec2f]]

	protected[text] var asciiCharacterWidths: Array[Float] = Array.ofDim[Float](asciiRange.upper)
	protected[text] var asciiCharacterPixelWidths: Array[Int] = Array.ofDim[Int](asciiRange.upper)
	protected[text] var asciiCharacterHeights: Array[Float] = Array.ofDim[Float](asciiRange.upper)
	protected[text] var asciiCharacterPixelHeights: Array[Int] = Array.ofDim[Int](asciiRange.upper)
	protected[text] var unicodeCharacterDimensions = new mutable.HashMap[Char, ReadVec2f]
	protected[text] var unicodeCharacterPixelDimensions = new mutable.HashMap[Char, ReadVec2i]

	protected[text] var _maxCharacterDimensions = Vec2f(0.0f, 0.0f)
	protected[text] var _maxCharacterDimensionsPixels = Vec2i(0,0)

	protected[text] var needsSolidification = false

	private val initializationTimer = Metrics.timer("AdaptiveBitmappedFont.init")

	protected[text] val psf: Float = EngineCore.pixelScaleFactor
	protected[text] val _fontMetrics = FontMetrics((fontHelper.fm.getMaxAscent * psf).toInt, (fontHelper.fm.getMaxDescent * psf).toInt, (fontHelper.fm.getHeight * psf).toInt, (font.getSize * psf).toInt, ((fontHelper.fm.charWidth(' ') + 1) * psf).toInt)
	override def fontMetrics = _fontMetrics



	if (glyphSources.nonEmpty) {init() }

	override def characterAdvancePixels(c: Char): Int = (fontHelper.fm.charWidth(c) * psf).toInt

	def init() {
		if (backingTextureBlock.isSentinel) {
			textureBlock.minFilter = GL11.GL_LINEAR_MIPMAP_LINEAR
			textureBlock.magFilter = GL11.GL_LINEAR
		}

		Noto.fine("Creating adaptive bitmapped font")

		initializationTimer.timeStmt {

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

			if (System.getenv("DEBUG_FONTS") == "true") {
				cacheFontImages()
			}

			Noto.fine("After font creation, percent available : " + (textureBlock.availableSpace / (textureBlock.width * textureBlock.height).toFloat))
		}
	}

	def deriveWithPtSize(fontSize : Int) = {
		// TODO: make it possible for derived fonts to be pre-cached (assuming we get that working again)
		derivedBySize.getOrElseUpdate(fontSize, new AdaptiveBitmappedFont(font.deriveFont(fontSize.toFloat), Nil, textureBlock, pixelFont))
	}

	def cacheFontImages() = {
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
	}

	def characterTexCoords(c: Char): Array[ReadVec2f] = {
		if (asciiRange.contains(c)) {
			asciiTexCoords(c)
		} else {
			ensureUnicodeCharAdded(c)
			unicodeTexCoords(c)
		}
	}

	/**
	  * Character width on a [0,1] scale, generally representing the proportion of the height
	  */
	override def characterWidthProportional(c: Char) = {
		if (asciiRange.contains(c)) {
			asciiCharacterWidths(c)
		} else {
			ensureUnicodeCharAdded(c)
			unicodeCharacterDimensions(c).x
		}
	}

	/**
	  * Character height on a [0,1] scale
	  */
	override def characterHeightProportional(c: Char) = {
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
