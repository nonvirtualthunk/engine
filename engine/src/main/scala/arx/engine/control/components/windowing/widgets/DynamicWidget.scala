package arx.engine.control.components.windowing.widgets

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 6/4/17
  * Time: 2:56 PM
  */

import arx.Prelude._
import arx.core.datastructures.Watcher

import scalaxy.loops._
import arx.core.vec._
import arx.engine.control.components.windowing.Widget

abstract class DynamicWidget[T](parentis : Widget) extends Widget(parentis){
	def currentContent : T
	def generateWidgets(current : T) : List[Widget]
	def positionWidgets(widgets : List[Widget]) : Unit


	val watcher = Watcher(currentContent)
	var generatedWidgets = generateWidgets(watcher.last)
	private var constructed = false
	positionWidgets(generatedWidgets)

	override def updateSelf(): Unit = {
		if (constructed && watcher.hasChanged) {
			val content = watcher.last
			generatedWidgets.foreach(w => w.close())
			generatedWidgets = generateWidgets(content)
			positionWidgets(generatedWidgets)
			_isModified = true
		}
	}

	constructed = true
}
