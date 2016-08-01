package arx.gui2.rendering

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/30/13
 * Time: 1:02 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.vec.ReadVec4f
import arx.core.vec.Vec4f
import arx.graphics.AVBO
import arx.graphics.Image
import arx.gui2.Widget
import arx.gui2.WindowingSystem2
import arx.resource.ResourceManager.image

class WidgetBackgroundComponentBase(imageFunc : (Widget) => Image) extends WidgetRenderingComponent {
	case class ImageMetric ( borderPixelWidth : Int , centerColor : ReadVec4f )
	val imageMetrics = memoize( (image:Image,seg:Boolean) => {
		if ( image.sentinel ) {
			ImageMetric(0,Vec4f.One)
		} else {
			var borderWidth = 0
			while ( borderWidth < image.width && image(borderWidth,image.height / 2,3) > 0 ) { borderWidth += 1}
			val centerColor = image.colorAtV4( image.width - 1 , 0 )

			if (seg && borderWidth >= image.width - 1) { Noto.warn("Old style segmented image detected, " + image.resourcePath) }
			ImageMetric(borderWidth, centerColor)
		}
	} )
	val BlankImage = image("default/blank.png")

	def draw(widget: Widget, vbo: AVBO, context: WindowingSystem2.RenderingContext, beforeChildren : Boolean ) {
		val img : Image = imageFunc(widget)

		if ( img.sentinel ) { return }
		val backColor : ReadVec4f = widget.backgroundColor
		val edgeColor : ReadVec4f = widget.edgeColor


		val seg : Boolean = widget.segmentedBackground

		val ww : Float = roundf(context.toPixelScaleX(widget.x + widget.width) - context.toPixelScaleX(widget.x))
		val wh : Float = roundf(context.toPixelScaleY(widget.y + widget.height) - context.toPixelScaleY(widget.y))

		val metrics = imageMetrics(img,seg)
		val tc = context.textureBlock(img)
		val tx = tc(0).x
		val ty = tc(0).y
		val tw = tc(2).x - tc(0).x
		val th = tc(2).y - tc(0).y

		val pixelScale = widget.pixelScale
		if ( beforeChildren ) {
			if ( seg ) {
				val blankTC = context.textureBlock( BlankImage )
				val coff = metrics.borderPixelWidth*pixelScale-1 //center offset
				if ( widget.drawCenterBackground ) {
					drawQuad(vbo,context,coff,coff,ww-coff*2,wh-coff*2,blankTC(0).x,blankTC(0).y,0.0f,0.0f,metrics.centerColor * backColor)
				}
			} else {
				drawQuad(vbo,context,0.0f,0.0f,ww,wh,tx,ty,tw,th,backColor)
			}
		} else {
			if ( seg ) {
				var cornerWidth = (img.width / 2) * pixelScale
				var cornerHeight = (img.height / 2) * pixelScale
				var verticalPercent = 1.0f
				var horizontalPercent = 1.0f
				if ( cornerWidth > ww / 2 ) {
					horizontalPercent = (ww / 2) / cornerWidth
					cornerWidth = roundf(ww / 2).toInt
				}
				if ( cornerHeight > wh / 2 ) {
					verticalPercent = (wh / 2) / cornerHeight
					cornerHeight = roundf(wh / 2).toInt
				}
				val sideWidth = ww - cornerWidth * 2.0f
				val sideHeight = wh - cornerHeight * 2.0f

				//Corner Texture coordinates
				val ctx = tx
				val cty = ty + tw * 0.5f + (tw * 0.5f * (1.0f - verticalPercent))
				val ctw = tw * 0.5f * horizontalPercent
				val cth = th * 0.5f * verticalPercent

				//Horizontal Side Texture coordinates
				val hstx = tx + tw * 0.5f
				val hsty = ty + th * 0.5f + (tw * 0.5f * (1.0f - verticalPercent))
				val hstw = 0
				val hsth = cth

				//Vertical Side Texture Coordinates
				val vstx = tx
				val vsty = ty
				val vstw = ctw
				val vsth = 0

				drawQuad(vbo,context,0.0f,0.0f,									cornerWidth,cornerHeight,ctx,cty,ctw,cth,edgeColor) //Top Left Corner
				drawQuad(vbo,context,ww - cornerWidth,0.0f,					cornerWidth,cornerHeight,ctx+ctw,cty,-ctw,cth,edgeColor) //Top Right Corner
				drawQuad(vbo,context,ww - cornerWidth,wh - cornerHeight,	cornerWidth,cornerHeight,ctx+ctw,cty+cth,-ctw,-cth,edgeColor) //Bottom Right Corner
				drawQuad(vbo,context,0.0f,wh - cornerHeight,					cornerWidth,cornerHeight,ctx,cty+cth,ctw,-cth,edgeColor) //Top Left Corner

				if ( ww > cornerWidth * 2 ) {
					drawQuad(vbo,context,cornerWidth,0.0f,						sideWidth,cornerHeight,hstx,hsty,hstw,hsth,edgeColor)
					drawQuad(vbo,context,cornerWidth,wh - cornerHeight,	sideWidth,cornerHeight,hstx,hsty+hsth,hstw,-hsth,edgeColor)
				}
				if ( wh > cornerHeight * 2 ) {
					drawQuad(vbo,context,0.0f,cornerHeight,					cornerWidth,sideHeight,vstx,vsty,vstw,vsth,edgeColor)
					drawQuad(vbo,context,ww - cornerWidth,cornerHeight,	cornerWidth,sideHeight,vstx+vstw,vsty,-vstw,vsth,edgeColor)
				}

			}
		}
	}
}

object WidgetBackgroundComponent extends WidgetBackgroundComponentBase((w) => w.backgroundImage)
//object WidgetBorderComponent extends WidgetBackgroundComponentBase((w) => w.borderImage)