package arx.gui2.rendering

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/3/13
 * Time: 10:16 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.application.Application
import arx.core.Moddable
import arx.core.math.Rectf
import arx.core.vec.Cardinals._
import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec3f
import arx.core.vec.ReadVec4f
import arx.engine.EngineCore
import arx.graphics.AVBO
import arx.graphics.helpers.TTextLayouter.LayoutParameters
import arx.graphics.helpers.{RichText, DeprecatedTextLayouter}
import arx.graphics.text.TBitmappedFont
import arx.gui2.Widget
import arx.gui2.WindowingSystem2.RenderingContext

class WidgetTextRenderingComponent(
				val text : Moddable[String],
				val font : Moddable[TBitmappedFont],
				val fontSize : Moddable[Float],
				val area : Moddable[Rectf],
				val fontColor : Moddable[ReadVec4f],
				val orientFromTop : Moddable[Boolean],
				val textAlignment : Moddable[Int],
				val windowingSystemDimensions : Moddable[ReadVec3f]
) extends WidgetRenderingComponent {
	val layouter = new DeprecatedTextLayouter

	val layoutFor = memoizeSingle( ( params : LayoutParameters ) => layouter.layOutText(params) )
	def currentLayout = layoutFor( LayoutParameters(RichText(text.resolve()),font,fontSize,area,1.0f,windowingSystemDimensions.x / EngineCore.windowWidth,textAlignment) )

	def lineHeight = layouter.lineHeight(font,fontSize)
	def lineSpacing = layouter.lineSpacing(font,fontSize)

	/**
	 * Determines the index in the text that most closesly corresponds to the given relative point. If the point
	 * is before the start of the text, -1 will be returned, if after, text.size will be returned.
	 */
	def intersectedIndex ( relativePoint : ReadVec2f ) : Int = {
		val points = currentLayout.points
		if ( points.isEmpty ) { -1 }
		else if ( relativePoint.y < points.head.y ) { -1 }
		else {
			val resolvedText : String = text
			val resolvedFont : TBitmappedFont = font
			val resolvedFontSize : Float = fontSize

			for ( (point,index) <- points.zipWithIndex ; if index < resolvedText.size  ; char = resolvedText(index) ) {
				val w = layouter.charWidth(char,resolvedFont,resolvedFontSize)
				val h = layouter.charHeight(char,resolvedFont,resolvedFontSize)
				if ( relativePoint.y < point.y ) { return index - 1 }
				else if ( relativePoint.y <= point.y + h && relativePoint.x < point.x + w / 2 ) {
					return index
				}
			}
			resolvedText.size
		}
	}

	def singleLineTextDimensions = {
		//TODO: Windowing system ratio
		layoutFor(LayoutParameters(RichText(text),font,fontSize,WidgetTextRenderingComponent.VeryLargeRect,1.0f,windowingSystemDimensions.x / EngineCore.windowWidth,Left) ).dimensions
	}

	def draw(widget: Widget, vbo: AVBO, context: RenderingContext, beforeChildren: Boolean) = {
		if ( beforeChildren ) {
			widget.selfBoundedBlock(context) {
				val layout = currentLayout

				val resolvedText : String = text
				val resolvedFont : TBitmappedFont = font
				val resolvedFontSize : Float = fontSize
				val resolvedColor : ReadVec4f = fontColor

				val bounds = context.boundsStack.head

				var index = 0
				val iter = layout.points.iterator
				var break = false
				while ( iter.hasNext && ! break && index < resolvedText.size ) {
					val point = iter.next()
					val char = resolvedText(index)
					if ( ! char.isWhitespace ) {
						val x = roundf( context.toPixelScaleX(point.x) )
						val y = roundf( context.toPixelScaleY(point.y) )
						val rawW = context.toPixelScaleX(layouter.charWidth(char,resolvedFont,resolvedFontSize))
						val rawH = context.toPixelScaleY(layouter.charHeight(char,resolvedFont,resolvedFontSize))
						val w = floorf( rawW + 0.0001f)
						val h = floorf( rawH + 0.0001f)

//						val w = resolvedFont.characterWidthPixels(char) * 1.0f
//						val h = resolvedFont.characterHeightPixels(char) * 1.0f
//						val pw = resolvedFont.characterWidthPixels(char)
//						val ph = resolvedFont.characterHeightPixels(char)

						val ey = effectiveY(context,y)
						if ( ey < bounds.y + bounds.h && ey + h > bounds.y ) {
							val tc = resolvedFont.characterTexCoords(char)
							val tx = tc(0).x// + 100.0f
							val ty = tc(0).y// + 100.0f
							val tw = tc(2).x - tc(0).x
							val th = tc(2).y - tc(0).y
							drawQuad(vbo,context,x,y,w,h,tx,ty,tw,th,resolvedColor)
						} else { break = true }

					}

					index += 1
				}
			}
		}
	}
}

object WidgetTextRenderingComponent{
	val VeryLargeRect = new Rectf(0.0f,0.0f,10000000.0f,10000000.0f)
}
