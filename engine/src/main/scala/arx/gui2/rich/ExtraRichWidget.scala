package arx.gui2.rich

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/1/14
 * Time: 8:44 AM
 */

import arx.application.Noto
import arx.core.ImplicitModdable._
import arx.core.representation.ConfigValue
import arx.graphics.Image
import arx.gui2.Widget
import arx.resource.ResourceManager.image


object ExtraRichWidget {
//	implicit def toRich(w: Widget) = new SMLEnrichedWidget (w)

	class ConfiguredEnrichedWidget(val widget: Widget) extends AnyVal {

		//+====================+ SML Interface +====================+
		def setFromSML(sml: ConfigValue, overwrite: Boolean): Unit = {
			widget.id = sml.id.strOrElse (widget.id)

			val xSml = if (sml.hasField ("x")) {sml.field ("x")} else if (sml.hasField ("pos")) {sml.pos.arr (0)} else {ConfigValue.Sentinel}
			val ySml = if (sml.hasField ("y")) {sml.field ("y")} else if (sml.hasField ("pos")) {sml.pos.arr (1)} else {ConfigValue.Sentinel}
			if (xSml.nonEmptyValue) {
				xSml.str match {
					case "centered" => widget.x = () => (widget.parentOE.clientWidth - widget.width) * 0.5f
					case "right-aligned" => widget.x = () => widget.parentOE.clientWidth - widget.width
					case s if s.endsWith ("px") => widget.x = widget.windowingSystem.widgetUnitsPerPixelX * s.dropRight (2).toFloat
					case _ => widget.x = xSml.float
				}
			}
			if (ySml.nonEmpty) {
				ySml.str match {
					case "centered" => widget.y = () => (widget.parentOE.clientHeight - widget.height) * 0.5f
					case "bottom-aligned" => widget.y = () => widget.parentOE.clientHeight - widget.height
					case s if s.endsWith ("px") => widget.y = widget.windowingSystem.widgetUnitsPerPixelY * s.dropRight (2).toFloat
					case _ => widget.y = ySml.float
				}
			}

			//		width = sml.width.floatOrElse(width)
			//		height = sml.height.floatOrElse(height)
			dimensionFromSML (sml.width, w = true)
			dimensionFromSML (sml.height, w = false)
			widget.pixelScale = sml.pixelScale.intOrElse (widget.pixelScale)

			if (sml.background.nonEmptyValue) {
				val imgStr = sml.background.str
				if (imgStr == "none") {
					widget.backgroundImage = Image.Sentinel
				} else {
					widget.backgroundImage = image (imgStr)
				}
			}
			if (sml.backgroundColor.nonEmptyValue) {widget.backgroundColor = sml.backgroundColor.v4}
			widget.dataString = sml.data.strOrElse (widget.dataString)

			if (sml.children.nonEmptyValue) {
				val smlWithIds = if (sml.children.isArr) {sml.children.arr.map (s => s.id.strOrElse ("------") -> s)}
				else {sml.children.fields.toList}
				for ((cid, child) <- smlWithIds) {
					val childWidget = widget.children.find (_.id == cid) match {
						case Some (w) => w
						case None => {
							val widgetType = child.widget.strOrElse ("widget")
							Widget.createWidgetWithTypeString (widgetType, widget)
						}
					}
					childWidget.id = cid


					new ConfiguredEnrichedWidget(childWidget).setFromSML (child, overwrite)
				}
			}

			if (sml.scrollBar.boolOrElse (false)) {
				widget.enableScrollBar ()
			}
		}

		protected def dimensionFromSML(sml: ConfigValue, w: Boolean) = {
			if (sml.nonEmpty) {
				val unwrapped = sml.unwrapped
				val convUnwrapped = unwrapped match {
					case i: Int => i.toFloat
					case o => o
				}

				convUnwrapped match {
					case str: String => {
						if (str.endsWith ("%")) {
							try {
								widget.parent match {
									case Some (p) =>
										if (w) {widget.width = () => p.clientWidth * (str.dropRight (1).toFloat * 0.01f)}
										else {widget.height = () => p.clientHeight * (str.dropRight (1).toFloat * 0.01f)}
									case None => Noto.warn ("Percentage width being applied to parentless widget")
								}
							} catch {
								case e: Exception => Noto.warn ("Percentage for width was malfored :" + str)
							}
						} else {
							Noto.warn ("Width string could not be interpreted : " + str)
						}
					}
					case f: Float => {
						if (f < 0.0f) {
							widget.parent match {
								case Some (p) =>
									if (w) {widget.width = () => p.clientWidth + f}
									else {widget.height = () => p.clientHeight + f}
								case None => Noto.warn ("Negative, relative width being applied to parentless widget")
							}
						} else {
							if (w) {widget.width = f}
							else {widget.height = f}
						}
					}
					case _ => Noto.warn ("Unknown sml value type for setting width")
				}
			}
		}
	}

}