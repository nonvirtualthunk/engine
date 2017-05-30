package arx.engine.graphics.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.application.Noto
import arx.core.math.Rectf
import arx.core.vec.ReadVec4f
import arx.core.vec.Vec2i
import arx.core.vec.Vec4f
import arx.engine.EngineCore
import arx.engine.control.components.windowing.Widget
import arx.engine.control.data.WindowingData
import arx.engine.graphics.data.WindowingGraphicsData
import arx.graphics.Image
import arx.graphics.TToImage
import arx.resource.ResourceManager

import scalaxy.loops._

class BackgroundRenderer(WD : WindowingGraphicsData) extends WindowingRenderer(WD) {
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
	val BlankImage : TToImage = ResourceManager.image("default/blank.png")

	override def render(widget: Widget, beforeChildren : Boolean): List[WQuad] = {
		val DD = widget.drawing
		import DD._

		val seg = true

		if (drawBackground) {
			val img = backgroundImage.map(_.image).getOrElse(WD.defaultBackgroundImage)

			val ww = effectiveDimensions.x
			val wh = effectiveDimensions.y
			val pixelScale = backgroundPixelScale * EngineCore.pixelScaleFactor.toInt

			val metrics = imageMetrics(img,true)

			if (beforeChildren) {
				if (!drawAsForegroundBorder) {
					if ( seg ) {
						val coff = metrics.borderPixelWidth*pixelScale-1 //center offset
						if ( drawCenterBackground ) {
							List(WQuad(Rectf(coff, coff, ww - coff*2, wh - coff*2), BlankImage, metrics.centerColor * backgroundColor))
						} else {
							Nil
						}
					} else {
						List(WQuad(Rectf(0.0f,0.0f,ww,wh),img,backgroundColor))
					}
				} else {
					Nil
				}
			} else {
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
				val ctx = 0.0f
				val cty = 1.0f - 0.5f * verticalPercent
				val ctw = 0.5f * horizontalPercent
				val cth = 0.5f * verticalPercent

				//Horizontal Side Texture coordinates
				val hstx = 0.5f
				val hsty = 0.5f + (0.5f * (1.0f - verticalPercent))
				val hstw = 0
				val hsth = cth

				//Vertical Side Texture Coordinates
				val vstx = 0.0f
				val vsty = 0.0f
				val vstw = ctw
				val vsth = 0

				val cornerTR = Rectf(ctx,cty,ctw,cth)
				var ret = List(
					WQuad(Rectf(0.0f,0.0f,cornerWidth,cornerHeight),img, edgeColor, 0, cornerTR),
					WQuad(Rectf(ww - cornerWidth,0.0f,cornerWidth,cornerHeight),img, edgeColor, 90, cornerTR),
					WQuad(Rectf(ww - cornerWidth, wh - cornerHeight,cornerWidth,cornerHeight), img, edgeColor, 180, cornerTR),
					WQuad(Rectf(0.0f,wh - cornerHeight,cornerWidth,cornerHeight), img, edgeColor, 270, cornerTR)
				)

				if ( ww > cornerWidth * 2 ) {
					ret ::= WQuad(Rectf(cornerWidth,0.0f,sideWidth,cornerHeight),img,edgeColor,0, Rectf(hstx,hsty,hstw,hsth))
					ret ::= WQuad(Rectf(cornerWidth,wh - cornerHeight, sideWidth,cornerHeight),img,edgeColor,180, Rectf(hstx,hsty,hstw,hsth))
				}
				if ( wh > cornerHeight * 2 ) {
					ret ::= WQuad(Rectf(0.0f,cornerHeight,cornerWidth,sideHeight),img,edgeColor,0, Rectf(vstx,vsty,vstw,vsth))
					ret ::= WQuad(Rectf(ww - cornerWidth,cornerHeight,cornerWidth,sideHeight),img,edgeColor,180, Rectf(vstx,vsty,vstw,vsth))
				}

				DD.decorationBorderSize = Vec2i(metrics.borderPixelWidth * pixelScale,
					metrics.borderPixelWidth * pixelScale)

				ret.reverse
			}
		} else {
			Nil
		}
	}

	override def decorationBorderSize(widget: Widget) = {
		val DD = widget.drawing
		if (DD.drawBackground) {
			val img = DD.backgroundImage.map(_.image).getOrElse(WD.defaultBackgroundImage)

			val pixelScale = DD.backgroundPixelScale * EngineCore.pixelScaleFactor.toInt

			val metrics = imageMetrics(img,true)

			Some(Vec2i(metrics.borderPixelWidth * pixelScale,
				metrics.borderPixelWidth * pixelScale))
		} else {
			None
		}
	}
}
