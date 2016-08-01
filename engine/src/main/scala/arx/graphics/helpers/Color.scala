package arx.graphics.helpers

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 12/1/12
 * Time: 4:55 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude
import arx.core.vec.ReadVec3f
import arx.core.vec.ReadVec4f
import arx.core.vec.Vec3f
import arx.core.vec.Vec4f
import arx.gui2.widgets.HSB
import arx.gui2.widgets.HSBA

object Color {
	def apply ( r : Int , g : Int , b : Int , a : Int ) = Vec4f( r.toFloat/255.0f , g.toFloat/255.0f , b.toFloat/255.0f, a.toFloat/255.0f )
	def apply ( rgb : Int , a : Int ) = Vec4f( rgb.toFloat/255.0f , rgb.toFloat/255.0f , rgb.toFloat/255.0f, a.toFloat/255.0f )
	def apply ( rgb : Float , a : Float ) = Vec4f( rgb, rgb , rgb , a )

	def scaleRGB ( v : ReadVec4f, rgbMult : Float ) = Vec4f(v.r * rgbMult,v.g * rgbMult,v.b * rgbMult,v.a)

	val Black : ReadVec4f = this(0.0f,1.0f)
	val White : ReadVec4f = this(1.0f,1.0f)
	val Grey  : ReadVec4f = this(0.5f,1.0f)
	val Red : ReadVec4f = Vec4f(1.0f,0.0f,0.0f,1.0f)
	val Green : ReadVec4f = Vec4f(0.0f,1.0f,0.0f,1.0f)
	val Blue : ReadVec4f = Vec4f(0.0f,0.0f,1.0f,1.0f)

	def fromInt ( i : Int ) = {
		val r = ((i & 0xff000000) >>> 24) / 255.0f
		val g = ((i & 0x00ff0000) >>> 16) / 255.0f
		val b = ((i & 0x0000ff00) >>> 8) / 255.0f
		val a = (i & 0x000000ff) / 255.0f
		Vec4f(r,g,b,a)
	}


	def rand = Vec4f(Prelude.rand(0.0f,1.0f),Prelude.rand(0.0f,1.0f),Prelude.rand(0.0f,1.0f),1.0f)


	def RGBAtoHSBA ( rgba : ReadVec4f ) = {
		val hsb = RGBtoHSB(rgba)
		new HSBA(hsb.h,hsb.s,hsb.b,rgba.a)
	}
	def HSBAtoRGBA ( hsba : ReadVec4f ) = {
		val rgb = HSBtoRGB(hsba.rgb)
		Vec4f(rgb.r,rgb.g,rgb.b,hsba.a)
	}
	def HSBAtoRGBA ( hsba : HSBA ) = {
		val rgb = HSBtoRGB(hsba.hsb)
		Vec4f(rgb.r,rgb.g,rgb.b,hsba.a)
	}
	def HSBtoRGB ( hsb : ReadVec3f ) = {
		val hue = hsb.x
		val sat = hsb.y
		val bri = hsb.z

		var r = 0.0f
		var g = 0.0f
		var b = 0.0f
		if ( sat == 0.0f ) {
			r = bri
			g = bri
			b = bri
		} else {
			val h = (hue - math.floor(hue).toFloat) * 6.0f
			val f = h - math.floor(h).toFloat
			val p = bri * (1.0f - sat)
			val q = bri * (1.0f - sat * f)
			val t = bri * (1.0f - (sat * (1.0f - f)))
			h.toInt match {
				case 0 => {
					r = bri
					g = t
					b = p
				}
				case 1 => {
					r = q
					g = bri
					b = p
				}
				case 2 => {
					r = p
					g = bri
					b = t
				}
				case 3 => {
					r = p
					g = q
					b = bri
				}
				case 4 => {
					r = t
					g = p
					b = bri
				}
				case 5 => {
					r = bri
					g = p
					b = q
				}
				case _ =>
			}
		}

		Vec3f(r,g,b)
	}

	def RGBtoHSB ( v : Vec3f ) : HSB = RGBtoHSB(v.r,v.g,v.b)
	def RGBtoHSB ( v : Vec4f ) : HSB = RGBtoHSB(v.r,v.g,v.b)
	def RGBtoHSB ( r : Float , g : Float , b : Float ) : HSB = {
		val hsb = new HSB(1.0f,1.0f,1.0f)

		var cmax = if (r > g) { r } else { g }
		if ( b > cmax ) { cmax = b }
		var cmin = if ( r < g ) { r } else { g }
		if ( b < cmin ) { cmin = b }

		hsb.b = cmax
		if ( cmax != 0.0f ) {
			hsb.s = (cmax - cmin) / cmax
		} else {
			hsb.s = 0.0f
		}
		if ( hsb.s == 0.0f ) {
			hsb.h = 0.0f
		} else {
			val redc = (cmax - r) / (cmax - cmin)
			val greenc = (cmax - g) / (cmax - cmin)
			val bluec = (cmax - b) / (cmax - cmin)
			if ( r == cmax ) {
				hsb.h = bluec - greenc
			} else if ( g == cmax ) {
				hsb.h = 2.0f + redc - bluec
			} else {
				hsb.h = 4.0f + greenc - redc
			}
			hsb.h = hsb.h / 6.0f
			if ( hsb.h < 0.0f ) {
				hsb.h = hsb.h + 1.0f
			}
		}

		hsb.r = Prelude.clamp(hsb.r,0.0f,1.0f)
		hsb.g = Prelude.clamp(hsb.g,0.0f,1.0f)
		hsb.b = Prelude.clamp(hsb.b,0.0f,1.0f)
		hsb
	}
}