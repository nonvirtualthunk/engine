package arx.gui2.fabrica

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/9/13
 * Time: 8:02 AM
 */

import arx.Prelude._

//import scalaxy.loops._
//import arx.gui.{TGameEntityWidgetFabrica, WidgetView}
//import arx.graphics.Image
//import arx.game.logic.entities.core.{GameArchetype, GameEntity, TGameBase}
//import arx.application.configuration.Setting
//import arx.application.Noto
//import arx.gui2.Widget
//import arx.gui2.widgets.{ImageDisplayWidget, TextDisplayWidget}
//import arx.core.Moddable
//import arx.engine.entity.GameArchetype
//import arx.engine.entity.GameEntity
//import arx.engine.entity.TGameEntity

object WidgetFabrica {
//	val entityFabrica = pio[TGameEntityWidgetFabrica]
//
//	def widgetFor ( v : Any, parent : Widget, view : WidgetView.WidgetView) = {
//		v match {
//			case i : Int => Widget.Sentinel
//			case b : Byte => Widget.Sentinel
//			case f : Float => Widget.Sentinel
//			case s : String => widgetForString(s,parent,view)
//			case img : Image => widgetForImage(img,parent,view)
//			case ent : TGameEntity => widgetForGameBase(ent,parent,view)
//			case arch : GameArchetype => widgetForGameBase(arch,parent,view)
//			case setting : Setting[_] => Widget.Sentinel
//			case _ => Noto.warn("Widget fabrica received unexpected thing : " + v);Widget.Sentinel
//		}
//	}
//
//	def widgetForString ( str : Moddable[String], parent : Widget, view : WidgetView.WidgetView ) = {
//		val tw = new TextDisplayWidget(str,parent)
//		tw.matchTextDimensions()
//		tw.backgroundImage = image("ui/minimalistBorder_ne.png")
//
//		tw
//	}
//	def widgetForImage ( img : Moddable[Image], parent : Widget , view : WidgetView.WidgetView ) = {
//		ImageDisplayWidget(img,parent)
//	}
//	def widgetForGameBase[T <: TGameBase](ent: Moddable[T], parent: Widget, view: WidgetView.WidgetView) = {
//		entityFabrica.constructWidgetFor(ent,ent.resolve().getClass.asInstanceOf[Class[T]],parent,view)
//	}
}
