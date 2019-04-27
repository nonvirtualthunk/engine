package arx.engine.control.components.windowing.widgets

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.Moddable
import arx.core.datastructures.Watcher
import arx.core.function.MemoizingFunction
import arx.core.vec.Cardinals.Left
import arx.core.vec.ReadVec2i
import arx.core.vec.ReadVec4f
import arx.engine.EngineCore
import arx.engine.control.components.windowing.Widget
import arx.graphics.TextureBlock
import arx.graphics.helpers.{Color, RichText}
import arx.graphics.text.TBitmappedFont
import arx.resource.ResourceManager
import scalaxy.loops._

class TextDisplayWidget(parentis : Widget) extends Widget(parentis) {
	var text : Moddable[RichText] = Moddable(RichText(""))
	var fontScale = 1.0f
	var fontColor : Moddable[ReadVec4f] = Moddable( Color.Black )
	var font = none[FontWrapper]
	var textAlignment = Moddable(Left)
	var orientFromTop = Moddable(true)
	private var constructed = false

	// default to no background for text widgets
	this.drawing.drawBackground = false

	protected[windowing] val textWatcher = Watcher(text.resolve())
	protected[windowing] val fontScaleWatcher = Watcher(fontScale)
	override def isSelfModified = constructed && (textWatcher.hasChanged || fontScaleWatcher.hasChanged)

	def effectiveFontScale = fontScale * EngineCore.pixelScaleFactor
	constructed = true
}


trait FontWrapper {
	val font : MemoizingFunction[TextureBlock, TBitmappedFont] = memoize((tb : TextureBlock) => createFont(tb))
	protected def createFont(tb : TextureBlock) : TBitmappedFont
}
class FontByName(name : String) extends FontWrapper {
	override def createFont(tb: TextureBlock): TBitmappedFont = {
		ResourceManager.font(name, tb)
	}
}