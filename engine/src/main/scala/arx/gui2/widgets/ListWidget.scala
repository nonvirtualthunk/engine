package arx.gui2.widgets

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/14/13
 * Time: 12:07 PM
 */

import arx.Prelude._
import arx.core.Mod
import arx.core.Moddable
import arx.core.representation.ConfigValue
import arx.core.vec.ReadVec4f
import arx.engine.control.event.Event.MouseReleaseEvent
import arx.graphics.helpers.Color
import arx.gui2.ContainerWidget
import arx.gui2.Widget
import arx.core.ImplicitModdable._
import arx.engine.control.event.Event.Event

class ListWidget(parentis : Widget) extends Widget(parentis) with ContainerWidget {
	var values = Moddable( List[Any]() )
	var valueToWidget =  (a : Any,p:Widget) => Widget.Sentinel
	var selectedValues : List[Any] = Nil
	var mustHaveSelection = false
	var singleSelection = true
	var subWidgets = List[Widget]()
	var selectedColorMultiplier = Moddable(Color(0.5f,1.0f))

	def configure[T] ( values : Moddable[List[T]] , valueToWidget : (T,Widget) => Widget ) {
		this.values = values
		this.valueToWidget = (a:Any,w:Widget) => valueToWidget(a.asInstanceOf[T],w)

		selectedValues = selectedValues.filter( values.contains )
		if ( mustHaveSelection && selectedValues.isEmpty ) { selectedValues :::= values.headOption.toList }

		reposition()
	}

	val watcher = new Watcher(visualsHash _)
	def visualsHash = (values.resolve(),maxWidth.resolve(),maxHeight.resolve()).hashCode

	override protected def updateUI () {
		reposition()
	}

	def reposition () {
		if ( watcher.hasChanged ) {
			subWidgets.foreach( _.close() )
			subWidgets = values.map( s => valueToWidget(s,this) )

			for ( (w,s) <- subWidgets.zip(values) ) {
				w.backgroundColor = Mod(w.backgroundColor, (c:ReadVec4f) => if ( selectedValues.contains(s) ) { c * selectedColorMultiplier } else { c })
				w.onEvent {
					case MouseReleaseEvent(_,_,_) => {
						if ( singleSelection ) { setSelectedValues(List(s)) }
						else { setSelectedValues(s :: selectedValues) }
					}
				}
			}

			val maxWidth = subWidgets.map(_.width).max
			val maxHeight = subWidgets.map(_.height).max

			val uniformWidth = clamp(maxClientWidth,this.minWidth,this.maxClientWidth)
			val uniformHeight = math.min(maxHeight,this.maxClientHeight)

			for ( (w,i) <- subWidgets.zipWithIndex ) {
				w.width = uniformWidth
				w.height = uniformHeight
				w.y = i * uniformHeight - 0.0001f
			}

			val autoSelfWidth = uniformWidth + this.interstitialNearX + this.interstitialFarX + this.internalPaddingX * 2.0f
			this.width = clamp(autoSelfWidth,this.minWidth,this.maxWidth)
		}
	}

	def setSelectedValues (l : List[Any]): Unit = {
		selectedValues = l
		fireEvent( ListWidget.SelectedValuesChanged(l) )
	}

	//+====================+ SML Interface +====================+
	override def setFromSML(sml: ConfigValue, overwrite: Boolean): Unit = {
		super.setFromSML (sml, overwrite)
		if ( sml.values.nonEmpty ) {
			val l : List[String] = sml.values.arr.toList.map ( _.str )
			configure( l , (s:String,w:Widget) => new TextDisplayWidget(s,w) )
		}
	}

	override protected[gui2] def SMLTypeIdentifier: String = "list"
}

object ListWidget {
	case class SelectedValuesChanged (selectedValues : List[Any]) extends Event
}