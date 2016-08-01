package arx.gui2.widgets

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 1/6/13
 * Time: 2:03 PM
 * Created by nonvirtualthunk
 */

import arx.core.vec._
import arx.graphics.helpers.Color
import arx.gui2.Widget
import arx.gui2.arrangement.Orientation
import arx.resource.ResourceManager
import arx.core.ImplicitModdable._
import arx.core.Moddable
import arx.core.vec.Cardinals._
import ResourceManager.image

class ColorPickerWidget(parentis : Widget, orientation : Orientation.Orientation = Orientation.Horizontal) extends Widget(parentis) { self =>
	if ( orientation == Orientation.Horizontal ) {
		width = 31.0f
		height = 14.0f
	} else {
		width = 15.0f
		height = 23.0f
	}
	backgroundImage = image("ui/fancyBackgroundWhite_ns.png")
	pixelScale = 1
//	setPixelArtBorder(ResourceManager.image("ui/greenMinorStyledBorder.png"))
//	borderMultiplier = 1.5f

	var hsba = HSBA(1.0f,1.0f,1.0f,1.0f)

	def hueSliderValues = HSB(0.0f,hsba.s,hsba.b) :: HSB(1.0f,hsba.s,hsba.b) :: HSB(1.0f,hsba.s,hsba.b) :: HSB(0.0f,hsba.s,hsba.b) :: Nil
	val hueSlider = new HSBDisplayWidget(this,hueSliderValues _)

	def saturationSliderValues = HSB(hsba.h,0.0f,hsba.b) :: HSB(hsba.h,1.0f,hsba.b) :: HSB(hsba.h,1.0f,hsba.b) :: HSB(hsba.h,0.0f,hsba.b) :: Nil
	val saturationSlider = new HSBDisplayWidget(this,saturationSliderValues _)

	def brightnessSliderValues = HSB(hsba.h,hsba.s,0.0f) :: HSB(hsba.h,hsba.s,1.0f) :: HSB(hsba.h,hsba.s,1.0f) :: HSB(hsba.h,hsba.s,0.0f) :: Nil
	val brightnessSlider = new HSBDisplayWidget(this,brightnessSliderValues _)
	
	val sliders = hueSlider :: saturationSlider :: brightnessSlider :: Nil

	var index = 0
	for ( slider <- sliders ) {
		val curIndex = index
		slider.backgroundImage = image("ui/blackBorder_ns.png")
		slider.pixelScale = 1
		slider.backgroundColor = Color.Red
		if ( orientation == Orientation.Horizontal ) {
			slider.width = () => self.clientWidth - 13.5f
			slider.centerY = () => ((self.clientHeight) / 4.0f) * (curIndex+1).toFloat
			slider.x = 1.0f
		} else {
			slider.width = self.clientWidth _
			slider.height = 2.5f
			slider.y = () => colorSwatch.height + 1.0f + curIndex * (slider.height + 1.5f)
		}

		val marker = new ImageDisplayWidget(slider)
		marker.image = ResourceManager.image("ui/marker_small_white.png")
		marker.backgroundColor = () => {
			val achsb = activeColorHSB
			val b = achsb.b
			val newB = 1.0f - b
			Color.HSBAtoRGBA(Vec4f(0.0f,0.0f,newB,1.0f))
		}
		marker.maintainExactPixelScale = true
		marker.anchorTo = Bottom
		marker.height = slider.height * 0.25f
		marker.x = () => self.hsba.apply(curIndex) * slider.clientWidth - marker.width * 0.5f
		marker.dockToInteriorBottom()

		index += 1
	}

	val colorSwatch = new Widget(this)
	colorSwatch.backgroundImage = image("ui/fancyBackgroundWhite_ns.png")
	colorSwatch.pixelScale = 1
//	colorSwatch.setPixelArtBorder(ResourceManager.image("ui/minimalistBorderWhite.png"))
	colorSwatch.backgroundColor = () => { activeColor }
	colorSwatch.width = 10.0f
	colorSwatch.height = 10.0f
	if ( orientation == Orientation.Horizontal ) {
		colorSwatch.dockToInteriorRight(1.0f)
		colorSwatch.centerVertically()
	} else {
		colorSwatch.centerHorizontally()
	}


	hueSlider.consumeEvent {
		case HSBColorSelectedEvent(hsbValue) => {
			hsba.h = hsbValue.h
		}
	}
	saturationSlider.consumeEvent {
		case HSBColorSelectedEvent(hsbValue) => {
			hsba.s = hsbValue.s
		}
	}
	brightnessSlider.consumeEvent {
		case HSBColorSelectedEvent(hsbValue) => {
			hsba.b = hsbValue.b
		}
	}

	def setColorTo (rgba : ReadVec4i): Unit = {
		setColorTo(rgba / 255.0f)
	}
	def setColorTo (rgba : ReadVec4f): Unit = {
		val newHSB = Color.RGBAtoHSBA(rgba)
		setColorTo (newHSB)
	}
	def setColorTo (newHsb : HSBA): Unit = {
		hsba.h = newHsb.h
		hsba.s = newHsb.s
		hsba.b = newHsb.b
		hsba.a = newHsb.a
	}

	def activeColorHSB : HSB = hsba.hsb
	def activeColorHSBA : HSBA = HSBA(hsba)
	def activeColor = {
		Color.HSBAtoRGBA(hsba)
	}
	def activeColor4i = {
		val v = Color.HSBAtoRGBA(hsba)
		Vec4i((v.r * 255).toInt,(v.g * 255).toInt,(v.b * 255).toInt,(v.a * 255).toInt)
	}

	var watcher = new Watcher(() => activeColor)
	override protected def updateLogic(f: Float): Unit = {
		if (watcher.hasChanged) {
			this.fireEvent(HSBColorSelectedEvent(hsba))
		}
	}

	override protected def SMLTypeIdentifier: String = "color picker"
}

class HSB(ha:Float,sa:Float,ba:Float) extends Vec3f(ha,sa,ba) {
	def h = x
	def s = y
	override def b = z

	def h_= ( f : Float ) { x = f }
	def s_= ( f : Float ) { y = f }
	override def b_= ( f : Float ) { z = f }

	def toRGBA = Color.HSBtoRGB(this)

}

class HSBA(ha:Float,sa:Float,ba:Float,aa:Float) extends Vec4f(ha,sa,ba,aa) {
	def this() { this(0.0f,0.0f,0.0f,0.0f) }
	def h = r
	def s = g
//	override def b = b

	def h_= ( f : Float ) { r = f }
	def s_= ( f : Float ) { g = f }
//	override def b_= ( f : Float ) { z = f }

	def toRGBA = Color.HSBAtoRGBA(this)
	def toRGBAi = Vec4i(Color.HSBAtoRGBA(this) * 255)
	def hsb = HSB(h,s,b)
}

object HSB {
	def apply ( h : Float , s : Float , b : Float ) : HSB = new HSB(h,s,b)
	def apply ( v : ReadVec3f ) : HSB = apply(v.x,v.y,v.z)
}
object HSBA {
	def apply ( h : Float , s : Float , b : Float , a : Float) : HSBA = new HSBA(h,s,b,a)
	def apply ( v : ReadVec3f, a : Float) : HSBA = apply(v.x,v.y,v.z,a)
	def apply ( v : ReadVec4f ) : HSBA = apply(v.r,v.g,v.b,v.a)
}