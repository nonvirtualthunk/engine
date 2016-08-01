package arx.gui2.widgets

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/3/13
 * Time: 10:39 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.application.Application
import arx.application.Noto
import arx.core.Moddable
import arx.core.Moddable._
import arx.core.representation.ConfigValue
import arx.core.vec.ReadVec4f
import arx.graphics.text.AdaptiveBitmappedFont
import arx.gui2.rendering.WidgetTextRenderingComponent
import arx.gui2.Widget
import arx.gui2.WindowingSystem2
import arx.core.ImplicitModdable._
import arx.core.math.Rectf
import arx.graphics.helpers.Color
import arx.core.vec.Cardinals._
import arx.engine.EngineCore
import arx.engine.control.event.Event.MousePressEvent

class TextDisplayWidget(parentis:Widget) extends Widget(parentis) {
	def this ( text_ : Moddable[String] , p : Widget ) { this(p); text = text_ }

	var _text : Moddable[String] = ""
	def text = _text
	def text_= ( t : Moddable[String] ) { _text = t }
//	var font = Moddable( ResourceManager.getFont(WindowingSystem.defaultFont) )
	var fontSize = Moddable( 1.88888888f * EngineCore.pixelScaleFactor )
	var fontColor : Moddable[ReadVec4f] = Moddable( Color.Black )
	var font = () => windowingSystem.font
	var textAlignment = Moddable( Left )
	acceptsFocus = false

	var orientFromTop = Moddable(true)
	pixelScale = 1

	def textArea = Rectf( clientX, clientY, clientWidth, clientHeight )

	def matchTextDimensions () {
		interstitialNearX = 0.0f
		interstitialNearY = 0.0f
		interstitialFarX = 0.0f
		interstitialFarY = 0.0f
		width = () => textRenderer.singleLineTextDimensions.x + internalPaddingX * 2
		height = () => textRenderer.singleLineTextDimensions.y + internalPaddingY * 2 + 0.5f
	}

	def computedFontSize = {
		val fs = fontSize.resolve()
		font match {
			case adap : AdaptiveBitmappedFont => {
				if ( adap.pixelFont ) {
					// the exact height in pixels of our free-float font size
					val inPixels = windowingSystem.widgetHeightToPixelHeight(fs,round = false)
					val ratio = inPixels / adap.characterHeightPixels(' ')
					((fs / ratio) * roundf(ratio)).max(1)
				} else {
					fs
				}
			}
			case _ => fs
		}
	}

	val textRenderer = new WidgetTextRenderingComponent(
		forward(text _),
		forward(font _),
		forward(computedFontSize _),
		textArea _,
		forward(fontColor _),
		forward( orientFromTop _ ),
		forward( textAlignment _ ),
		Moddable(() => windowingSystem.dimensions))
	renderers :+= textRenderer

	override protected[gui2] def SMLTypeIdentifier = "text"

	//+====================+ SML Interface +====================+
	override def setFromSML(sml: ConfigValue,overwrite : Boolean) = {
		if ( sml.text.nonEmpty ) {
			val textString = sml.text.str
			if ( textString.contains("${") ) {
				var sections = Vector[Moddable[String]]()
				var continue = true
				var index = 0
				while ( continue ) {
					val nextStart = textString.indexOf("${",index)
					if ( nextStart >= 0 ) {
						if ( nextStart - index > 0 ) { sections :+= Moddable(textString.substring(index,nextStart)) }
						val nextEnd = textString.indexOf("}",nextStart)
						if ( nextEnd <= 0 ) { Noto.warn("No closing bracket for replacement in sml") }
						else {
							sections :+= Moddable(() => replacement( textString.substring(nextStart+2,nextEnd) ) match {
								case str : String => str
								case anyFunc : (() => Any) => anyFunc() match {
									case sRes : String => sRes
									case oRes => oRes.toString
								}
								case other => other.toString
							})
							index = nextEnd+1
						}
					} else {
						if (index != 0) {
							sections :+= Moddable(textString.substring(index))
						}
						continue = false
					}
				}

				if ( sections.size > 1 ) {
					_text = () => sections.map(_.resolve()).reduceLeft(_ + _)
				} else if ( sections.isEmpty ) {
					_text = ""
				} else {
					_text = sections.head
				}
			} else {
				_text = textString
			}
		}
		if (sml.textAlignment.nonEmpty) {
			sml.textAlignment.str.toLowerCase match {
				case "center" => textAlignment = Center
				case "left" => textAlignment = Left
				case "right" => textAlignment = Right
				case _ => Noto.warn("Invalid text alignment value : " + sml.textAlignment.str)
			}
		}
		fontColor = sml.fontColor.v4OrElse(fontColor)

		if ( sml.fontSize.nonEmpty ) {
			val sizeStr = sml.fontSize.str
			if ( sizeStr.endsWith("px") ) {
				fontSize = () => (sizeStr.dropRight(2).toFloat / EngineCore.pixelHeight) * windowingSystem.dimensions.y
			} else {
				fontSize = sizeStr.toFloat
			}
		}

		if ( sml.matchTextDimensions.boolOrElse(false) ) {
			matchTextDimensions()
		}

		if (sml.font.nonEmptyValue) {
			windowingSystem.loadFont(sml.font.str)
		}

		super.setFromSML (sml,overwrite)
	}

	onEvent {
		case MousePressEvent(button,pos,modifiers) => {
			val relativePosition = pos - absolutePosition.xy
			println(f"relative position $relativePosition")
			val idx = textRenderer.intersectedIndex(relativePosition.xy)
			if ( idx == -1 ) { println("\tpressed prior to text start") }
			else if ( idx >= text.resolve().size ) { println("\tpressed after text end") }
			else {
				println(f"\tchar pressed : ${text.resolve()(idx)}")
			}
		}
	}
}
