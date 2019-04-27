package arx.engine.control.components.windowing

import java.awt.font.{FontRenderContext, GlyphVector, TextLayout}
import java.awt.geom.AffineTransform
import java.awt.image.renderable.RenderableImage
import java.awt.{Color, Composite, Font, FontMetrics, Graphics, Graphics2D, GraphicsConfiguration, Image, Paint, Rectangle, RenderingHints, Shape, Stroke}
import java.awt.image.{BufferedImage, BufferedImageOp, ImageObserver, RenderedImage}
import java.text.AttributedCharacterIterator
import java.util

import arx.resource.ResourceManager
import org.scalatest.FlatSpec

class TextRenderingTests extends FlatSpec {

	"text" should "draw to custom graphics" in {

		val fontStream = ResourceManager.getResourceStream("fonts/OpenSans-Regular.ttf")
		val font = Font.createFont(Font.TRUETYPE_FONT,fontStream).deriveFont(Font.PLAIN,16)

		val frc = new FontRenderContext(new AffineTransform(), true, true)
		val layout = new TextLayout("This is a test", font, frc)

		val glyphVector = font.createGlyphVector(frc, "This is a test")


		val g = new TestGraphics2D
		layout.draw(g, 0.0f, 0.0f)
	}
}


class TestGraphics2D extends Graphics2D {
	override def draw(s: Shape): Unit = ???

	override def drawImage(img: Image, xform: AffineTransform, obs: ImageObserver): Boolean = ???

	override def drawImage(img: BufferedImage, op: BufferedImageOp, x: Int, y: Int): Unit = ???

	override def drawRenderedImage(img: RenderedImage, xform: AffineTransform): Unit = ???

	override def drawRenderableImage(img: RenderableImage, xform: AffineTransform): Unit = ???

	override def drawString(str: String, x: Int, y: Int): Unit = ???

	override def drawString(str: String, x: Float, y: Float): Unit = ???

	override def drawString(iterator: AttributedCharacterIterator, x: Int, y: Int): Unit = ???

	override def drawString(iterator: AttributedCharacterIterator, x: Float, y: Float): Unit = ???

	override def drawGlyphVector(g: GlyphVector, x: Float, y: Float): Unit = {
		g
	}

	override def fill(s: Shape): Unit = ???

	override def hit(rect: Rectangle, s: Shape, onStroke: Boolean): Boolean = ???

	override def getDeviceConfiguration: GraphicsConfiguration = ???

	override def setComposite(comp: Composite): Unit = ???

	override def setPaint(paint: Paint): Unit = ???

	override def setStroke(s: Stroke): Unit = ???

	override def setRenderingHint(hintKey: RenderingHints.Key, hintValue: Any): Unit = ???

	override def getRenderingHint(hintKey: RenderingHints.Key): AnyRef = ???

	override def setRenderingHints(hints: util.Map[_, _]): Unit = ???

	override def addRenderingHints(hints: util.Map[_, _]): Unit = ???

	override def getRenderingHints: RenderingHints = ???

	override def translate(x: Int, y: Int): Unit = ???

	override def translate(tx: Double, ty: Double): Unit = ???

	override def rotate(theta: Double): Unit = ???

	override def rotate(theta: Double, x: Double, y: Double): Unit = ???

	override def scale(sx: Double, sy: Double): Unit = ???

	override def shear(shx: Double, shy: Double): Unit = ???

	override def transform(Tx: AffineTransform): Unit = ???

	override def setTransform(Tx: AffineTransform): Unit = ???

	override def getTransform: AffineTransform = ???

	override def getPaint: Paint = ???

	override def getComposite: Composite = ???

	override def setBackground(color: Color): Unit = ???

	override def getBackground: Color = ???

	override def getStroke: Stroke = ???

	override def clip(s: Shape): Unit = ???

	override def getFontRenderContext: FontRenderContext = ???

	override def create(): Graphics = ???

	override def getColor: Color = ???

	override def setColor(c: Color): Unit = ???

	override def setPaintMode(): Unit = ???

	override def setXORMode(c1: Color): Unit = ???

	override def getFont: Font = ???

	override def setFont(font: Font): Unit = ???

	override def getFontMetrics(f: Font): FontMetrics = ???

	override def getClipBounds: Rectangle = ???

	override def clipRect(x: Int, y: Int, width: Int, height: Int): Unit = ???

	override def setClip(x: Int, y: Int, width: Int, height: Int): Unit = ???

	override def getClip: Shape = ???

	override def setClip(clip: Shape): Unit = ???

	override def copyArea(x: Int, y: Int, width: Int, height: Int, dx: Int, dy: Int): Unit = ???

	override def drawLine(x1: Int, y1: Int, x2: Int, y2: Int): Unit = ???

	override def fillRect(x: Int, y: Int, width: Int, height: Int): Unit = ???

	override def clearRect(x: Int, y: Int, width: Int, height: Int): Unit = ???

	override def drawRoundRect(x: Int, y: Int, width: Int, height: Int, arcWidth: Int, arcHeight: Int): Unit = ???

	override def fillRoundRect(x: Int, y: Int, width: Int, height: Int, arcWidth: Int, arcHeight: Int): Unit = ???

	override def drawOval(x: Int, y: Int, width: Int, height: Int): Unit = ???

	override def fillOval(x: Int, y: Int, width: Int, height: Int): Unit = ???

	override def drawArc(x: Int, y: Int, width: Int, height: Int, startAngle: Int, arcAngle: Int): Unit = ???

	override def fillArc(x: Int, y: Int, width: Int, height: Int, startAngle: Int, arcAngle: Int): Unit = ???

	override def drawPolyline(xPoints: Array[Int], yPoints: Array[Int], nPoints: Int): Unit = ???

	override def drawPolygon(xPoints: Array[Int], yPoints: Array[Int], nPoints: Int): Unit = ???

	override def fillPolygon(xPoints: Array[Int], yPoints: Array[Int], nPoints: Int): Unit = ???

	override def drawImage(img: Image, x: Int, y: Int, observer: ImageObserver): Boolean = ???

	override def drawImage(img: Image, x: Int, y: Int, width: Int, height: Int, observer: ImageObserver): Boolean = ???

	override def drawImage(img: Image, x: Int, y: Int, bgcolor: Color, observer: ImageObserver): Boolean = ???

	override def drawImage(img: Image, x: Int, y: Int, width: Int, height: Int, bgcolor: Color, observer: ImageObserver): Boolean = ???

	override def drawImage(img: Image, dx1: Int, dy1: Int, dx2: Int, dy2: Int, sx1: Int, sy1: Int, sx2: Int, sy2: Int, observer: ImageObserver): Boolean = ???

	override def drawImage(img: Image, dx1: Int, dy1: Int, dx2: Int, dy2: Int, sx1: Int, sy1: Int, sx2: Int, sy2: Int, bgcolor: Color, observer: ImageObserver): Boolean = ???

	override def dispose(): Unit = ???
}