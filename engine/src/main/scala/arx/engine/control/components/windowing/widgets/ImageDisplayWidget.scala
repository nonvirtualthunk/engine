package arx.engine.control.components.windowing.widgets

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 6/4/17
  * Time: 11:26 AM
  */

import arx.Prelude._
import arx.core.Moddable
import arx.core.datastructures.Watcher

import scalaxy.loops._
import arx.core.vec._
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.ImageDisplayWidget.{PositionStyle, ScalingStyle}
import arx.graphics.TToImage
import arx.graphics.helpers.Color
import arx.resource.ResourceManager

class ImageDisplayWidget(parentis: Widget) extends Widget(parentis){
	var image : Moddable[TToImage] = Moddable(ResourceManager.blankImage)
	var scalingStyle : ScalingStyle = ImageDisplayWidget.ActualSize(1.0f)
	var positionStyle : PositionStyle = ImageDisplayWidget.TopLeft
	var color : ReadVec4f = Color.White
	protected[windowing] var watcher = Watcher(image.image)

	override protected[windowing] def isSelfModified = watcher.hasChanged
}


object ImageDisplayWidget {
	sealed class ScalingStyle
	case object ScaleToFit extends ScalingStyle
	case class ActualSize(scaleFactor : Float) extends ScalingStyle

	sealed class PositionStyle
	case object TopLeft extends PositionStyle
	case object Center extends PositionStyle
}