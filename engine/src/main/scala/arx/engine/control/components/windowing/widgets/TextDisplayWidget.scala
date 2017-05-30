package arx.engine.control.components.windowing.widgets

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.Moddable
import arx.core.datastructures.Watcher
import arx.core.vec.Cardinals.Left
import arx.core.vec.ReadVec2i
import arx.core.vec.ReadVec4f
import arx.engine.EngineCore
import arx.engine.control.components.windowing.Widget
import arx.graphics.TextureBlock
import arx.graphics.helpers.Color
import arx.graphics.text.TBitmappedFont
import arx.resource.ResourceManager

import scalaxy.loops._

class TextDisplayWidget(parentis : Widget) extends Widget(parentis) {
	var text : Moddable[String] = Moddable("")
	var fontScale = EngineCore.pixelScaleFactor
	var fontColor : Moddable[ReadVec4f] = Moddable( Color.Black )
	var font = none[FontWrapper]
	var textAlignment = Moddable(Left)
	var orientFromTop = Moddable(true)

	protected[windowing] val textWatcher = Watcher(text.resolve())
	protected[windowing] val fontScaleWatcher = Watcher(fontScale)
	override def isModified = textWatcher.hasChanged || fontScaleWatcher.hasChanged
}


trait FontWrapper {
	val font = memoize((tb : TextureBlock) => createFont(tb))
	protected def createFont(tb : TextureBlock) : TBitmappedFont
}
class FontByName(name : String) extends FontWrapper {
	override def createFont(tb: TextureBlock): TBitmappedFont = {
		ResourceManager.font(name, tb)
	}
}