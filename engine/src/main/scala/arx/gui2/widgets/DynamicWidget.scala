package arx.gui2.widgets

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/30/14
 * Time: 6:38 PM
 */

import arx.Prelude._
import arx.core.Moddable
import arx.gui2.Widget

class DynamicWidget(parentis:Option[Widget]) extends Widget(parentis) {
	def this(p : Widget) { this(Option(p)) }

	var values = Moddable( List[Any]() )
	var valueToWidget =  (a : Any,p:Widget) => Widget.Sentinel
	var repositionWidgets = (l : List[Widget]) => {}
	var subWidgets = List[Widget]()

//	var valueHash : Option[(List[Any]) => Int] = None
//	val watcher = new Watcher(() => valueHash.map(f => f(values)).getOrElse(defaultVisualsHash))
	var watcher = new Watcher(defaultVisualsHash _)
	def defaultVisualsHash = (values.resolve(),maxWidth.resolve(),maxHeight.resolve()).hashCode

	def configure[T] ( values : Moddable[List[T]] , valueToWidget : (T,Widget) => Widget, repositionWidgets : (List[Widget]) => Unit ) {
		this.values = values
		this.valueToWidget = (a:Any,w:Widget) => valueToWidget(a.asInstanceOf[T],w)
		this.repositionWidgets = repositionWidgets

		recreateWidgets()
	}


	override protected def updateUI(): Unit = {
		recreateWidgets()
	}

	def recreateWidgets(): Unit = {
		if ( watcher.hasChanged ) {
			subWidgets.foreach( _.close() )
			subWidgets = values.map( s => valueToWidget(s,this) )

			repositionWidgets(subWidgets)
		}
	}

	override protected[gui2] def SMLTypeIdentifier: String = "dynamic"
}


class DynamicChildren(parentis:Widget) {
	parentis.onUpdateUI ::= updateUI _
	var values = Moddable( List[Any]() )
	var valueToWidget =  (a : Any,p:Widget) => Widget.Sentinel
	var repositionWidgets = (l : List[Widget]) => {}
	var subWidgets = List[Widget]()

	var watcher = new Watcher(defaultVisualsHash _)
	def defaultVisualsHash = (values.resolve(),parentis.maxWidth.resolve(),parentis.maxHeight.resolve()).hashCode

	def configure[T] ( values : Moddable[List[T]] , valueToWidget : (T,Widget) => Widget, repositionWidgets : (List[Widget]) => Unit ) {
		this.values = values
		this.valueToWidget = (a:Any,w:Widget) => valueToWidget(a.asInstanceOf[T],w)
		this.repositionWidgets = repositionWidgets

		recreateWidgets()
	}


	protected def updateUI(): Unit = {
		if ( watcher.hasChanged ) {
			recreateWidgets()
		}
	}

	def recreateWidgets(): Unit = {
		subWidgets.foreach( _.close() )
		subWidgets = values.map( s => valueToWidget(s,parentis) )

		repositionWidgets(subWidgets)
	}

}