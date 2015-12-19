package arx.graphics.text

import java.awt.AlphaComposite
import java.awt.Color
import java.awt.Font
import java.awt.FontMetrics
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.image.BufferedImage
import java.awt.image.Raster
import java.io._
import java.nio.ByteBuffer
import java.nio.IntBuffer

import arx.application.Application
import arx.core.vec.ReadVec2f
import arx.core.vec.Vec2f
import arx.graphics.GL
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL30

import scala.collection.mutable.HashMap

@SerialVersionUID(1L)
class BitmappedFont extends TBitmappedFont {
	def this ( fontFile : File ) {
		this()
		font = Font.createFont(Font.TRUETYPE_FONT,fontFile).deriveFont(64.0f)
		init()
	}
	def this ( fontStream : InputStream ) {
		this()
		font = Font.createFont(Font.TRUETYPE_FONT,fontStream).deriveFont(64.0f)
		init()
	}
	def this (fontName : String, fontStyle : Int) {
		this()
		font = new Font( fontName , fontStyle , 64 );
		init()
	}
	var font: Font = null
	val asciiStart = 32
	val asciiEnd = 132
	@transient var texture : Int = 0
	@transient var textureSlot: Int = 0

	var characterWidths = HashMap[Int,Float]()
	var _characterTexCoords = Array.ofDim[ReadVec2f](100,4)
	var buf: ByteBuffer = null
	var textureSize: Int = 0
	@transient var needsSolidification = false

	var maxCharacterDimensions_i = Vec2f(0.0f,0.0f)
	def maxCharacterDimensions : ReadVec2f = maxCharacterDimensions_i

	def bind (i: Int) {
		if ( needsSolidification ) { solidify() }
		textureSlot = i
		GL.bindTexture(textureSlot,texture)
	}
	def unbind () {
		if ( textureSlot >= 0 ) {
			textureSlot = -1
			GL.bindTexture(textureSlot,0)
		}
	}

	def characterTexCoords(c: Char): Array[ReadVec2f] = _characterTexCoords ( c.toInt - 32)

	def nextPowerOfTwo ( i : Int ) : Int = {
		var v = i - 1
		v |= v >> 1;v |= v >> 2;v |= v >> 4;v |= v >> 8;v |= v >> 16;
		v + 1
	}

	def solidify () {
		needsSolidification = if ( Application.openGLThread.get ) {
			texture = glGenTextures();
			GL.bindTexture(texture);
			glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_LINEAR)
			glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR)

			/* Code for when using minified pixel fonts at larger sizes */
//			glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST)
//			glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST)
//
//			var x = 0; while ( x < textureSize * textureSize ) {
//				val i = x * 4
//				val a = buf.get(i+3) & 0xff
//				if ( a > 0 ) {
//					buf.put(i+3,0xff.toByte)
//				}
//			x += 1}

			buf.rewind()
			//GLU.gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, textureSize, textureSize,  GL_RGBA, GL_UNSIGNED_BYTE, buf);
			glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,textureSize,textureSize,0,GL_RGBA,GL_UNSIGNED_BYTE,buf)
      		GL30.glGenerateMipmap(GL_TEXTURE_2D)
			false
		} else {
			true
		}
	}

	def init () {
		val start = System.currentTimeMillis()
		val backgroundColor: Color = new Color(1.0f,1.0f,1.0f,1.0f);
		val foregroundColor: Color = new Color(1.0f,1.0f,1.0f,1.0f);
		val fontSize: Int = getFontSize(font);
		textureSize = nextPowerOfTwo(fontSize*10);

		// create a buffered image to hold charset
		val image:BufferedImage = new BufferedImage(textureSize, textureSize, BufferedImage.TYPE_INT_ARGB);
		val g:Graphics2D = image.createGraphics();

		// Clear image with background color (make transparent if color has alpha value)
		g.setComposite(AlphaComposite.getInstance(AlphaComposite.CLEAR, 0.0f));

		g.setColor(backgroundColor);
		g.fillRect(0,0,textureSize,textureSize);

		// prepare to draw characters in foreground color
		g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1.0f));
		g.setColor(foregroundColor);

		g.setFont(font);
		g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
		g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON);
		g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);

		// get font measurements
		val fm: FontMetrics = g.getFontMetrics;
		val ascent: Int = fm.getMaxAscent;
		val descent: Int = fm.getMaxDescent

		// draw the grid of 100 characters
		for ( r <- 0 until 10 ; c <- 0 until 10 ){
			val ch: Char = (32 + ((r*10)+c)).toChar
			g.drawString( String.valueOf(ch), (c*fontSize), (r*fontSize)+ascent);
			characterWidths(ch) = fm.charWidth(ch).toFloat / fontSize.toFloat

			val w : Float = ((fm.charWidth(ch)-3).toFloat / textureSize.toFloat);
			val h : Float = ((fm.getHeight - 6).toFloat / textureSize.toFloat);
			val x : Float = ((c * fontSize + 2).toFloat / textureSize.toFloat);
			val y : Float = 1.0f - (((r+1)*fontSize - 3).toFloat / textureSize.toFloat);
			_characterTexCoords (r * 10 + c)(0) = Vec2f(x,y);
			_characterTexCoords (r * 10 + c)(1) = Vec2f(x+w,y);
			_characterTexCoords (r * 10 + c)(2) = Vec2f(x+w,y+h);
			_characterTexCoords (r * 10 + c)(3) = Vec2f(x,y+h);

			maxCharacterDimensions_i.x = math.max(maxCharacterDimensions_i.x,w*10.0f)
			maxCharacterDimensions_i.y = math.max(maxCharacterDimensions_i.y,h*10.0f)
		}
		g.dispose();

		buf = BufferUtils.createByteBuffer(4 * textureSize * textureSize);
		val ibuf: IntBuffer = buf.asIntBuffer();

		val rast: Raster = image.getData;

		for ( x <- 0 until textureSize ; y <- 0 until textureSize ) {
			ibuf.put((textureSize - y - 1) * textureSize + x , rast.getSample(x,y,0) | ( rast.getSample(x,y,1) << 8 ) | (rast.getSample(x,y,2) << 16) | (rast.getSample(x,y,3) << 24) );
		}

		println("Font size: " + fontSize + " Texture Size: " + textureSize);
		println("Time taken : " + ((System.currentTimeMillis() - start) / 1000.0) + "s")
		solidify()
	}

	def getFontSize(font: Font): Int = {
		val isAntiAliased: Boolean = true
		val usesFractionalMetrics: Boolean = true
		val image: BufferedImage = new BufferedImage(64, 64, BufferedImage.TYPE_INT_ARGB)
		val g: Graphics2D = image.createGraphics
		g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1.0f))
		g.setFont(font)
		g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, if (isAntiAliased) RenderingHints.VALUE_TEXT_ANTIALIAS_ON else RenderingHints.VALUE_TEXT_ANTIALIAS_OFF)
		g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, if (usesFractionalMetrics) RenderingHints.VALUE_FRACTIONALMETRICS_ON else RenderingHints.VALUE_FRACTIONALMETRICS_OFF)
		g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
		val fm: FontMetrics = g.getFontMetrics
		val ascent: Int = fm.getMaxAscent
		val descent: Int = fm.getMaxDescent
		val advance: Int = fm.charWidth('W')
		val leading: Int = fm.getLeading
		val fontHeight: Int = ascent + descent + (leading / 2)
		val fontWidth: Int = advance
		val maxCharSize: Int = Math.max(fontHeight, fontWidth)
		maxCharSize
	}

	def characterWidth(c: Char) = characterWidths(c.toInt)
	def characterHeight(c: Char) = {
		val tc = characterTexCoords(c)
		math.abs((tc(2).y - tc(0).y) * 10.0f)
	}
}