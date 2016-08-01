package arx.gui2.rendering

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/30/13
 * Time: 1:02 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._

import arx.graphics.AVBO
import arx.gui2.{WindowingSystem2, Widget}
import arx.core.vec.{ReadVec2f, ReadVec4f}
import arx.gui2.WindowingSystem2.RenderingContext

abstract class WidgetRenderingComponent {
	def draw (widget : Widget, vbo : AVBO , context: WindowingSystem2.RenderingContext, beforeChildren : Boolean )

	def effectiveY( context : WindowingSystem2.RenderingContext, wy : Float ) = wy*context.scale.y+context.translationPixelsY
	def effectiveX( context : WindowingSystem2.RenderingContext, wx : Float ) = wx*context.scale.x+context.translationPixelsX

	def cx ( context : RenderingContext, x : Float ) = context.toPixelScaleX(x)
	def cy ( context : RenderingContext, y : Float ) = context.toPixelScaleY(y)

	def drawQuad (vbo : AVBO,context : WindowingSystem2.RenderingContext,wx: Float,wy: Float,ww: Float,wh: Float,tc : Array[ReadVec2f],c : ReadVec4f) {
		drawQuad(vbo,context,wx,wy,ww,wh,tc(0).x,tc(0).y,tc(2).x - tc(0).x,tc(2).y - tc(0).y,c)
	}

	def drawQuadConvert (vbo : AVBO,context : WindowingSystem2.RenderingContext,wx: Float,wy: Float,ww: Float,wh: Float,tc : Array[ReadVec2f],c : ReadVec4f) {
		drawQuad(vbo,context,roundf(context.toPixelScaleX(wx)),roundf(context.toPixelScaleY(wy)),roundf(context.toPixelScaleX(ww)),roundf(context.toPixelScaleY(wh)),tc,c)
	}

	def drawQuad (vbo : AVBO,context : WindowingSystem2.RenderingContext,wx: Float,wy: Float,ww: Float,wh: Float,tx: Float,ty: Float,tw: Float,th: Float,c : ReadVec4f) {
		val scale = context.scale
		val offx = context.translationPixelsX.toInt
		val offy = context.translationPixelsY.toInt


		val vi = vbo.incrementVertexOffset(4)
		val ii = vbo.incrementIndexOffset(6)
		//		println(f"Drawing quad : $wx,$wy -> ${wx + ww},${wy + wh}")
		//		println(f"Drawing quad, raw : ${wx*scale.x+off.x},${wy*scale.y+off.y} -> ${(wx+ww)*scale.x+off.x},${(wy+wh)*scale.y+off.y}")

		vbo.setA(WindowingSystemAttributeProfile2.TC,vi,tx,ty+th)
		vbo.setA(WindowingSystemAttributeProfile2.V,vi,roundf(wx*scale.x+offx).toInt,roundf(wy*scale.y+offy).toInt)
		vbo.setA(WindowingSystemAttributeProfile2.C,vi, c)
		context.setBounds(vbo,WindowingSystemAttributeProfile2.B,vi)

		vbo.setA(WindowingSystemAttributeProfile2.TC,vi+1,tx+tw,ty+th)
		vbo.setA(WindowingSystemAttributeProfile2.V,vi+1,roundf((wx + ww)*scale.x +offx).toInt,roundf(wy*scale.y+offy).toInt)
		vbo.setA(WindowingSystemAttributeProfile2.C,vi+1, c)
		context.setBounds(vbo,WindowingSystemAttributeProfile2.B,vi+1)

		vbo.setA(WindowingSystemAttributeProfile2.TC,vi+2,tx+tw,ty)
		vbo.setA(WindowingSystemAttributeProfile2.V,vi+2,roundf((wx + ww)*scale.x+offx).toInt,roundf((wy + wh)*scale.y+offy).toInt)
		vbo.setA(WindowingSystemAttributeProfile2.C,vi+2, c)
		context.setBounds(vbo,WindowingSystemAttributeProfile2.B,vi+2)

		vbo.setA(WindowingSystemAttributeProfile2.TC,vi+3,tx,ty)
		vbo.setA(WindowingSystemAttributeProfile2.V,vi+3,roundf(wx*scale.x+offx).toInt,roundf((wy + wh)*scale.y+offy).toInt)
		vbo.setA(WindowingSystemAttributeProfile2.C,vi+3, c)
		context.setBounds(vbo,WindowingSystemAttributeProfile2.B,vi+3)

		vbo.setIQuad(ii,vi)

	}

	def drawQuadWS (vbo : AVBO,context : WindowingSystem2.RenderingContext,wx: Float,wy: Float,ww: Float,wh: Float,tx: Float,ty: Float,tw: Float,th: Float,c : ReadVec4f) {
		val scale = context.scale
		val offx = context.translationPixelsX.toInt
		val offy = context.translationPixelsY.toInt

		val nx = context.toPixelScaleX(wx)
		val ny = context.toPixelScaleX(wy)
		val fx = context.toPixelScaleX(wx + ww)
		val fy = context.toPixelScaleY(wy + wh)

		val vi = vbo.incrementVertexOffset(4)
		val ii = vbo.incrementIndexOffset(6)
		//		println(f"Drawing quad : $wx,$wy -> ${wx + ww},${wy + wh}")
		//		println(f"Drawing quad, raw : ${wx*scale.x+off.x},${wy*scale.y+off.y} -> ${(wx+ww)*scale.x+off.x},${(wy+wh)*scale.y+off.y}")

		vbo.setA(WindowingSystemAttributeProfile2.TC,vi,tx,ty+th)
		vbo.setA(WindowingSystemAttributeProfile2.V,vi,roundf(nx*scale.x+offx).toInt,roundf(ny*scale.y+offy).toInt)
		vbo.setA(WindowingSystemAttributeProfile2.C,vi, c)
		context.setBounds(vbo,WindowingSystemAttributeProfile2.B,vi)

		vbo.setA(WindowingSystemAttributeProfile2.TC,vi+1,tx+tw,ty+th)
		vbo.setA(WindowingSystemAttributeProfile2.V,vi+1,roundf(fx*scale.x +offx).toInt,roundf(ny*scale.y+offy).toInt)
		vbo.setA(WindowingSystemAttributeProfile2.C,vi+1, c)
		context.setBounds(vbo,WindowingSystemAttributeProfile2.B,vi+1)

		vbo.setA(WindowingSystemAttributeProfile2.TC,vi+2,tx+tw,ty)
		vbo.setA(WindowingSystemAttributeProfile2.V,vi+2,roundf(fx*scale.x+offx).toInt,roundf(fy*scale.y+offy).toInt)
		vbo.setA(WindowingSystemAttributeProfile2.C,vi+2, c)
		context.setBounds(vbo,WindowingSystemAttributeProfile2.B,vi+2)

		vbo.setA(WindowingSystemAttributeProfile2.TC,vi+3,tx,ty)
		vbo.setA(WindowingSystemAttributeProfile2.V,vi+3,roundf(nx*scale.x+offx).toInt,roundf(fy*scale.y+offy).toInt)
		vbo.setA(WindowingSystemAttributeProfile2.C,vi+3, c)
		context.setBounds(vbo,WindowingSystemAttributeProfile2.B,vi+3)

		vbo.setIQuad(ii,vi)

	}
}
