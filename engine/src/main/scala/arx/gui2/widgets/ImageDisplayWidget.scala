package arx.gui2.widgets

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/9/13
 * Time: 7:46 AM
 */

import arx.Prelude._
import arx.core.Moddable
import arx.core.representation.ConfigValue
import arx.core.vec.ReadVec4f
import arx.core.vec.Vec4f
import arx.graphics.AVBO
import arx.graphics.Image
import arx.gui2.rendering.WidgetRenderingComponent
import arx.gui2.Widget
import arx.gui2.WindowingSystem2
import arx.resource.ResourceManager
import arx.core.ImplicitModdable._
import arx.core.vec.Cardinals._
import Moddable._

class ImageDisplayWidget (parentis:Widget) extends Widget (parentis){
	var image : Moddable[Image] = ResourceManager.defaultImage
	var color : Moddable[ReadVec4f] = Vec4f.One
	var maintainAspectRatio = true
	var maintainExactPixelScale = false
	var fixedRatio : Option[Float] = None
	var anchorTo = Center
	backgroundImage = Image.Sentinel

	renderers :+= new ImageWidgetComponent(forward(image _),forward(color _),forward(maintainAspectRatio _),forward(maintainExactPixelScale _),forward(fixedRatio _),forward(anchorTo _))

//	override def height_=(h: Moddable[Float]): Unit = maxHeight = h
//	override def width_=(w: Moddable[Float]): Unit = maxWidth = w

//	protected def lowerRatio = {
//		val img = backgroundImage : Image
//		val wRatio = maxWidth / img.width
//		val hRatio = maxHeight / img.height
//		math.min(wRatio,hRatio)
//	}
//
//	override def height: Float = {
//		if ( ! maintainAspectRatio ) { maxHeight }
//		else { backgroundImage.height * lowerRatio }
//	}
//	override def width: Float = {
//		if ( ! maintainAspectRatio ) { maxWidth }
//		else { backgroundImage.width * lowerRatio }
//	}

	//+====================+ SML Interface +====================+
	override def setFromSML(sml: ConfigValue, overwrite: Boolean): Unit = {
		super.setFromSML (sml, overwrite)
		if ( sml.image.nonEmpty ) {
			if (sml.image.str.contains("${")) {
				val rawStr = sml.image.str
				val replacementStr = rawStr.substring(rawStr.indexOf("${")+"${".length,rawStr.lastIndexOf("}"))
				image = () => replacement(replacementStr) match {
					case img : Image => img
					case s : String => ResourceManager.image(s)
					case _ => ResourceManager.defaultImage
				}
			} else {
				image = ResourceManager.image(sml.image.str)
			}
		}
		maintainAspectRatio = sml.maintainAspectRatio.boolOrElse(maintainAspectRatio)
		maintainExactPixelScale = sml.maintainExactPixelScale.boolOrElse(maintainExactPixelScale)
		if (sml.fixedRatio.nonEmptyValue) {
			fixedRatio = Some(sml.fixedRatio.float)
		}
		if (sml.anchorTo.nonEmptyValue) {
			anchorTo = sml.anchorTo.str match {
				case "center" => Center
				case "left" => Left
				case "right" => Right
				case _ => anchorTo
			}
		}
	}

	override protected[gui2] def SMLTypeIdentifier: String = "image"
}

object ImageDisplayWidget{
	def apply (image : Moddable[Image],parentis:Widget) = {
		val iw = new ImageDisplayWidget(parentis)
		iw.image = image
		iw
	}
}


class ImageWidgetComponent(image : Moddable[Image],
									color : Moddable[ReadVec4f],maintainAspectRatio:Moddable[Boolean],
									maintainExactPixelScale:Moddable[Boolean],
								   fixedRatio : Moddable[Option[Float]],
									anchorTo:Moddable[Int]) extends WidgetRenderingComponent {
	def draw(widget: Widget, vbo: AVBO, context: WindowingSystem2.RenderingContext, beforeChildren: Boolean): Unit = {
		val fixRatio : Boolean = maintainAspectRatio

		val img = image : Image
		val baseW = widget.clientWidth
		val baseH = widget.clientHeight

		val baseX = context.toPixelScaleX(widget.clientX)
		val baseY = context.toPixelScaleY(widget.clientY)
		val tc = context.textureBlock(image.resolve())

		val (x,y,w,h) = if ( fixRatio ) {
			val wRatio = baseW / img.width
			val hRatio = baseH / img.height
			val lowerRatio = math.min(wRatio,hRatio)

			val rawW = context.toPixelScaleX(img.width * lowerRatio)
			val rawH = context.toPixelScaleY(img.height * lowerRatio)

			val (w,h) = if (fixedRatio.nonEmpty) {
				val r = fixedRatio.get
				(img.width * r,img.height * r)
			} else if (maintainExactPixelScale) {
				val r = (rawW / img.width).floor.max(1.0f)
				(img.width * r,img.height * r)
			} else {
				(rawW,rawH)
			}

			val ax = anchorTo.resolve() match {
				case Left => 0.0f
				case Right => context.toPixelScaleX( baseW ) - w
				case _ => ( context.toPixelScaleX( baseW ) - w ) * 0.5f
			}
			val ay = anchorTo.resolve() match {
				case Top => 0.0f
				case Bottom => context.toPixelScaleX( baseH ) - h
				case _ => ( context.toPixelScaleX( baseH ) - h ) * 0.5f
			}

			(baseX + ax,baseY + ay,w,h)
		} else {
			(baseX,baseY,baseW,baseH)
		}

		drawQuad(vbo,context,x,y,w,h,tc,color)
	}
}