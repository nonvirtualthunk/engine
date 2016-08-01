package arx.gui2.widgets

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/9/13
 * Time: 7:57 AM
 */

import arx.Prelude._
import arx.core.representation.ConfigValue
import arx.engine.control.event.Event._
import arx.graphics.helpers.Color
import arx.gui2.arrangement.Orientation
import arx.gui2.ContainerWidget
import arx.gui2.Widget
import arx.core.ImplicitModdable._

class RadioSelectionWidget(parentis:Widget) extends Widget(parentis:Widget) with ContainerWidget {
	protected var _activeState : Option[Any] = None
	protected var states = List[Any]()
	protected var stateToWidget : (Any,Widget) => Widget = (a,w) => Widget.Sentinel

	protected var compact = false
	protected var subWidgets = List[Widget]()
	protected var orientation : Orientation.Orientation = Orientation.Horizontal

	def activeState = _activeState

	def configure[T] ( states : List[T] , stateToRepresentation : (T,Widget) => Widget ) {
		this.states = states
		this.stateToWidget = (a:Any,w:Widget) => stateToRepresentation(a.asInstanceOf[T],w)

//		val subWidgets = for ( state <- states ) yield {
//			val w = WidgetFabrica.widgetFor( stateToRepresentation(state) , this , WidgetView.Icon )
//			w -> state
//		}

		_activeState = _activeState match {
			case Some(oldState) if states.contains(oldState) => Some(oldState)
			case None => states.headOption
		}

		reposition()
	}

	val watcher = new Watcher(visualsHash _)
	def visualsHash = (states,compact,maxWidth,maxHeight,orientation).hashCode

	override protected def updateLogic(f: Float): Unit = {
		reposition()
	}

	def reposition () {
		if ( watcher.hasChanged ) {
			children.foreach( _.close() )
			subWidgets = states.map( s => stateToWidget(s,this) )

			for ( (w,s) <- subWidgets.zip(states) ) {
				w.backgroundColor = () => if ( _activeState == Some(s) ) { Color(0.5f,1.0f) } else { Color(1.0f,1.0f) }
				w.onEvent {
					case MouseReleaseEvent(_,_,_) => _activeState = Some(s)
				}
			}
//			val pcntPerW = 1.0f/subWidgets.size
//			if ( compact ) {
			val maxWidth = subWidgets.map(_.width).max
			val maxHeight = subWidgets.map(_.height).max
			if ( orientation == Orientation.Horizontal ) {
				val uniformWidth = math.min(maxWidth,this.maxClientWidth/subWidgets.length)
				val uniformHeight = math.min(maxHeight,this.maxClientHeight)

				var prev = 0.0f
				for ( (w,i) <- subWidgets.zipWithIndex ) {
					w.width = uniformWidth
					w.height = uniformHeight
					w.x = prev
					prev = w.x + uniformWidth
				}
			} else {
				val uniformWidth = math.min(maxWidth,this.maxClientWidth)
				val uniformHeight = math.min(maxHeight,this.maxClientHeight/subWidgets.length)

				for ( (w,i) <- subWidgets.zipWithIndex ) {
					w.width = uniformWidth
					w.height = uniformHeight
					w.y = i * uniformHeight
				}
			}
//			} else {
//				subWidgets.zip(states).zipWithIndex.foreach { case ((w,state),i) => {
//						w.x = i * pcntPerW * maxClientWidth
//						w.width = pcntPerW * maxClientWidth
//						w.height = maxClientHeight
//						w.backgroundColor = () => if (activeState == Some(state)) { Vec4f(0.5f,0.5f,0.5f,1.0f) } else { Color.White }
//						w match {
//							case t : TextDisplayWidget => t.textAlignment = Center
//							case _ =>
//						}
//					}
//				}
//			}
		}
	}

	//+====================+ SML Interface +====================+
	override def setFromSML(sml: ConfigValue, overwrite: Boolean): Unit = {
		super.setFromSML (sml, overwrite)
		compact = sml.compact.boolOrElse(compact)
		if ( sml.states.nonEmpty ) {
			if ( sml.states.isArr ) {
				val l : List[String] = sml.states.arr.toList.map ( _.str )
				configure( l , (s:String,w:Widget) => new TextDisplayWidget(s,w) )
			} else {
				val stateKeys = sml.states.fields.keys.toList.sortBy( s => sml.states.fields(s).order.intOrElse(0) )
				configure(stateKeys,(key:String,parentis:Widget) => {
					Widget.createFromSML(sml.states.fields(key),parentis)
				})
			}
		}
		orientation = Orientation.withName(sml.orientation.strOrElse(orientation.toString).capitalize)
	}

	override protected[gui2] def SMLTypeIdentifier: String = "radio"
}

class Watcher[T] ( value : () => T ) {
	var last : T = value()
	def hasChanged = {
		val cur = value()
		if ( last != cur ) {
			last = cur
			true
		} else {
			false
		}
	}
}

object RadioSelectionWidget{
	def apply (parentis:Widget) = {

	}
}