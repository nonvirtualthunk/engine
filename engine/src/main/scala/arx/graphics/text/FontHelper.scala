package arx.graphics.text

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/15/13
 * Time: 9:04 AM
 * Created by nonvirtualthunk
 */

import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, Color, Font, Point, RenderingHints}
import java.awt.geom.AffineTransform
import java.io.InputStream

import arx.core.math.Recti
import arx.engine.EngineCore
import arx.graphics.Image
import arx.graphics.SubImageView


class FontHelper(font: Font, pixelFont: Boolean = false) {
	private val underlyingGraphicsWidth = (font.getSize*2.2*EngineCore.pixelScaleFactor).toInt
	private val underlyingGraphicsHeight = (font.getSize*2.2*EngineCore.pixelScaleFactor).toInt
	val bufferedImage = new BufferedImage(underlyingGraphicsWidth,underlyingGraphicsHeight, BufferedImage.TYPE_INT_ARGB)
	val g = bufferedImage.createGraphics

	val backgroundColor: Color = new Color(1.0f,1.0f,1.0f,1.0f)
	val foregroundColor: Color = new Color(1.0f,1.0f,1.0f,1.0f)

	g.setFont(font)
	g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
	if ( ! pixelFont ) {
		g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
		g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
	} else {
		g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_OFF)
		g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_OFF)
//		g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
	}
	g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
	g.setTransform(AffineTransform.getScaleInstance(EngineCore.pixelScaleFactor, EngineCore.pixelScaleFactor))


	val fm = g.getFontMetrics
	val ascent = fm.getMaxAscent
	val descent = fm.getMaxDescent
	val lineHeight = fm.getHeight

	var retImage : Image = Image.withDimensions(underlyingGraphicsWidth,underlyingGraphicsHeight)

	def clearCanvas () {
		// Clear image with background color (make transparent if color has alpha value)
		g.setComposite(AlphaComposite.getInstance(AlphaComposite.CLEAR,0.0f))
		g.setColor(backgroundColor)
		g.fillRect(0,0,underlyingGraphicsWidth,underlyingGraphicsHeight)
	}

	/**
	 * Draws a character to an Image and returns it. The image returned
	 * will be invalidated as soon as drawChar is next called
	 * @param ch the character to render
	 * @return an image representation of the character
	 */
	def drawChar ( ch : Char ) = {
		clearCanvas()
		// prepare to draw characters in foreground color
		g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1.0f))
		g.setColor(foregroundColor)


		val originX = underlyingGraphicsWidth / 4
		g.drawString( String.valueOf(ch) , originX / EngineCore.pixelScaleFactor , fm.getAscent )

		val stringBounds = fm.getStringBounds(s"$ch", 0, 1, g)
		val cWidth = stringBounds.getWidth.ceil.toInt * EngineCore.pixelScaleFactor.toInt + 1
		val cHeight = stringBounds.getHeight.ceil.toInt * EngineCore.pixelScaleFactor.toInt + 1

		val rast = bufferedImage.getData
		var maxX = 0
		var minX = originX + cWidth-1
		var x = 0 ; while ( x < originX + cWidth ) {
			var y = 0 ; while ( y < cHeight ) {
				var q = 0; while ( q < 4 ) {
					val rastByte = rast.getSample(x,y,q).toByte
					if ( q == 3 && rastByte != 0 ) {
						maxX = math.max(x,maxX)
						minX = math.min(x,minX)
					}
					retImage(x,cHeight - y - 1,q) = rastByte
				q += 1}
			y += 1}
		x += 1}
//		Image.save(retImage, s"/tmp/rawchars/$ch.png")
		val retSubImage = new SubImageView(retImage,Recti(minX,0,math.max(maxX+1-minX,1),cHeight))
//		Image.save(retSubImage, s"/tmp/chars/$ch.png")
		retSubImage
	}

	def advance ( ch : Char ) = {
		fm.charWidth(ch)
	}
}

protected class AWTFontGlyphSource(val fontHelper : FontHelper) extends GlyphSource {
	def lineHeightPixels = fontHelper.lineHeight

	override def canProvideGlyphFor(char: Char): Boolean = true
	override def glyphFor(char: Char): Image = {
		fontHelper.drawChar(char)
	}
}

object AWTFontGlyphSource {
	def apply (fontStream : InputStream, basePointSize : Int, pixelFont : Boolean = false) : AWTFontGlyphSource = {
		val f = Font.createFont(Font.TRUETYPE_FONT,fontStream).deriveFont(Font.PLAIN,basePointSize)
		AWTFontGlyphSource(f,pixelFont)
	}
	def apply (font : Font, pixelFont : Boolean) : AWTFontGlyphSource = {
		new AWTFontGlyphSource(new FontHelper(font, pixelFont))
	}
}