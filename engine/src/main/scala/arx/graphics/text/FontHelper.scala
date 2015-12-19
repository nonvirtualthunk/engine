package arx.graphics.text

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/15/13
 * Time: 9:04 AM
 * Created by nonvirtualthunk
 */

import java.awt.image.BufferedImage
import java.awt.AlphaComposite
import java.awt.Color
import java.awt.Font
import java.awt.RenderingHints
import java.io.InputStream

import arx.core.math.Recti
import arx.graphics.Image
import arx.graphics.SubImageView


class FontHelper(font:Font,pixelFont:Boolean = false,drop : Int = 0) {
	def pixelSize = imgSize
	val bufferedImage = new BufferedImage((font.getSize*2).toInt,(font.getSize*2).toInt, BufferedImage.TYPE_INT_ARGB)
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


	val fm = g.getFontMetrics
	val ascent = fm.getMaxAscent
	val descent = fm.getMaxDescent

	val imgSize : Int = math.max(fm.charWidth('W')+3,fm.getMaxAscent+fm.getMaxDescent+fm.getLeading/2+3) //fm. //font.getSize * 1.5).toInt
	var retImage : Image = Image.withDimensions(imgSize,imgSize)

	def clearCanvas () {
		// Clear image with background color (make transparent if color has alpha value)
		g.setComposite(AlphaComposite.getInstance(AlphaComposite.CLEAR,0.0f))
		g.setColor(backgroundColor)
		g.fillRect(0,0,imgSize,imgSize)
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

		g.drawString( String.valueOf(ch) , 0 , ascent )

		val cWidth = if ( ch == 'f' ) { fm.charWidth(ch) + 2 } else if ( ch == 'T' ) { fm.charWidth(ch) - 1 } else { fm.charWidth(ch) }
		val cHeight = fm.getHeight


		val rast = bufferedImage.getData
		var maxX = 0
		var minX = cWidth-1
		var x = 0 ; while ( x < cWidth ) {
			var y = 0 ; while ( y < cHeight ) {
				var q = 0; while ( q < 4 ) {
					val rastX = x
					val rastY = math.max(y,0)
					val rastByte = rast.getSample(rastX,rastY,q).toByte
					if ( q == 3 && rastByte != 0 ) {
						maxX = math.max(x,maxX)
						minX = math.min(x,minX)
					}
					retImage(x,cHeight - y - 1,q) = rastByte
				q += 1}
			y += 1}
		x += 1}
		val retSubImage = new SubImageView(retImage,Recti(minX,0 + drop,math.max(maxX+1-minX,1),cHeight - drop))
		retSubImage
	}

	def advance ( ch : Char ) = {
		fm.charWidth(ch)
	}
}

protected class AWTFontGlyphSource extends GlyphSource {
	var font : Font = null
	var pixelFont : Boolean = false
	var drop : Int = 0
	var fontHelper : FontHelper = null

	def init (): Unit = {
		fontHelper = new FontHelper(font,pixelFont,drop)
	}

	override def canProvideGlyphFor(char: Char): Boolean = true
	override def glyphFor(char: Char): Image = {
		fontHelper.drawChar(char)
	}
}

object AWTFontGlyphSource {
	def apply (fontStream : InputStream, basePointSize : Int, pixelFont : Boolean = false, drop : Int = 0) : AWTFontGlyphSource = {
		val f = Font.createFont(Font.TRUETYPE_FONT,fontStream).deriveFont(Font.PLAIN,basePointSize)
		AWTFontGlyphSource(f,pixelFont,drop)
	}
	def apply (font : Font, pixelFont : Boolean, drop : Int) : AWTFontGlyphSource = {
		val src = new AWTFontGlyphSource
		src.font = font
		src.pixelFont = pixelFont
		src.drop = drop
		src.init()
		src
	}
}