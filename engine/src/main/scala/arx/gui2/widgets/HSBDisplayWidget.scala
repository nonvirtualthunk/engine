package arx.gui2.widgets

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 1/6/13
 * Time: 12:45 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.core.Moddable
import arx.core.mat.Mat4x4
import arx.core.vec.ReadVec2f
import arx.core.vec.Vec3f
import arx.engine.control.event.Event.MouseDragEvent
import arx.engine.control.event.Event.MousePressEvent
import arx.engine.control.event.Event.UIEvent
import arx.graphics.shader.Shader
import arx.graphics.AVBO
import arx.graphics.AttributeProfile
import arx.graphics.GL
import arx.graphics.helpers.HSBA
import arx.gui2.Widget
import arx.resource.ResourceManager
import org.lwjgl.opengl.GL11
import org.lwjgl.opengl.GL15

class HSBDisplayWidget(parentWidget:Widget,hsbValues : Moddable[List[Vec3f]]) extends OpenGLWidget(parentWidget){
	val shader = ResourceManager.shader("shaders/windowing/ColorPicker")
	val vbo = new AVBO(HSBAttributeProfile)

//	setPixelArtBorder(ResourceManager.getImage("ui/blackBorder.png"))
//	borderMultiplier = 0.5f


	vbo.incrementVertexOffset(4)
	vbo.incrementIndexOffset(6)
	val VA = HSBAttributeProfile.VertexAttributeIndex
	val HSBA = HSBAttributeProfile.HSBAttributeIndex

	vbo.setA(VA,0,0.0f,0.0f,0.0f)
	vbo.setA(VA,1,1.0f,0.0f,0.0f)
	vbo.setA(VA,2,1.0f,1.0f,0.0f)
	vbo.setA(VA,3,0.0f,1.0f,0.0f)

	vbo.setIQuad(0,0)

	val watcher = new Watcher(hsbValues.resolve)
	var first = true

	def drawGL() {
		shader.bind()

		shader.setUniform("ProjectionMatrix",GL.ortho(0.0f,1.0f,0.0f,1.0f,-1.0f,1.0f),tolerateAbsence = false)
		shader.setUniform("ModelViewMatrix",Mat4x4.Identity,tolerateAbsence = false)

		GL.glSetState(GL11.GL_DEPTH_TEST,enable = false)
		GL.glSetState(GL11.GL_CULL_FACE,enable = false)

		if (watcher.hasChanged || first) {
			first = false
			for ( i <- 0 until 4 ) {
				vbo.setA(HSBA,i,hsbValues(i))
			}

			vbo.solidify(GL15.GL_STREAM_DRAW)
		}

		vbo.drawElements(GL11.GL_TRIANGLES)
		Shader.unbind()
	}

	onEvent {
		case MousePressEvent(button,pos,modifiers) => {
			colorHitAtPos(pos)
		}
		case MouseDragEvent(pos,delta,button,modifiers) => {
			colorHitAtPos(pos)
		}
	}

	def colorHitAtPos(pos:ReadVec2f) {
		val xf = ((pos.x - absolutePosition.x) / clientWidth).clamp(0.0f,1.0f)
		val yf = ((pos.y - absolutePosition.y) / clientHeight).clamp(0.0f,1.0f)

		val hsb = hsbValues.resolve()
		//Interpolate the left pair, and the right pair, based on the yf
		val X1 = mix(hsb(3),hsb(0),yf)
		val X2 = mix(hsb(2),hsb(1),yf)

		//Interpolate the left with the right based on the xf
		val end = mix(X1,X2,xf)

		fireEvent( HSBColorSelectedEvent(new HSBA(end.x,end.y,end.z,1.0f)) )
	}
}

case class HSBColorSelectedEvent ( hsba : HSBA ) extends UIEvent

object HSBAttributeProfile extends AttributeProfile( List("vertex" -> (3,GL11.GL_FLOAT), "hsb" -> (3,GL11.GL_FLOAT)) ) {
	val VertexAttributeIndex = this.attributesByName("vertex")
	val HSBAttributeIndex = this.attributesByName("hsb")
	vertexAttributeIndex = this.attributesByName("vertex")
}
