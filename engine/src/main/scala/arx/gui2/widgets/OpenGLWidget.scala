package arx.gui2.widgets

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/9/13
 * Time: 4:39 PM
 * To change this template use File | Settings | File Templates.
 */

import java.nio.IntBuffer

import arx.application.Noto
import arx.core.mat.ReadMat4x4
import arx.core.math.Rectf
import arx.core.vec.ReadVec3f
import arx.core.vec.Vec3f
import arx.core.vec.Vec4f
import arx.engine.EngineCore
import arx.graphics._
import arx.graphics.shader.Shader
import arx.gui2.Widget
import arx.gui2.WindowingSystem2.RenderingContext
import arx.gui2.rendering.WidgetRenderingComponent
import arx.gui2.widgets.OpenGLWidget.OpenGLRenderer
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11._
import arx.core.ImplicitModdable._

abstract class OpenGLWidget(parentis : Widget) extends Widget(parentis) {
	var viewport: Rectf = Rectf(0.0f,0.0f,100.0f,100.0f)
	var viewportBuffer: IntBuffer = BufferUtils.createIntBuffer(16)

	var currentBounds : Rectf = Rectf(0.0f,0.0f,100.0f,100.0f)
	var topLevelBounds : Rectf = Rectf(0.0f,0.0f,100.0f,100.0f)

	backgroundImage = Image.Sentinel

	renderers :+= OpenGLRenderer

	def subDrawGL () {
		val startViewport = GL.viewport
		val apos = absolutePosition
//		val ax = windowingSystem.widgetCoordinatesToPixelCoordinatesX(apos.x + clientX)
//		val ay = windowingSystem.widgetCoordinatesToPixelCoordinatesY(apos.y + clientY)
//		val ax2 = windowingSystem.widgetCoordinatesToPixelCoordinatesX(apos.x + clientX + clientWidth)
//		val ay2 = windowingSystem.widgetCoordinatesToPixelCoordinatesY(apos.y + clientY + clientHeight)
		val ax = lastOffset.x
		val ay = lastOffset.y
		val ax2 = lastOffset.x + windowingSystem.widgetWidthToPixelWidth(clientX + clientWidth,false)
		val ay2 = lastOffset.y + windowingSystem.widgetHeightToPixelHeight(clientY + clientHeight,false)
		val aw = ax2 - ax
		val ah = ay2 - ay

		val baseViewRect = Rectf(ax,ay,aw,ah)
		val viewRect = currentBounds intersect baseViewRect
//		for ( i <- 0 until 4 ) {
//			viewRect(i) = viewRect(i) / topLevelBounds(2+i%2)
//		}
		viewport = Rectf(
			(startViewport.x + viewRect.x).floor + 0,
			(EngineCore.pixelHeight - (startViewport.y + viewRect.y + viewRect.height - 0)).floor + 1,
			(viewRect.w).round + 0,
			(viewRect.h).round + 0
		)
		// This viewport is fudged a bit, the "- 0" should in theory be "- 1", and there shouldn't need to be a +1
		// for the height, but in practice this seems to work better, so here we are.

		// ^ Previous comment was earlier, have adjusted to -1 for y, +2 for h, prevents some annoying visual bugs
		// but may have issues in other cases


		GL.setViewport(viewport)
		viewportBuffer.rewind()
		viewportBuffer.put(0,viewport.x.toInt)
		viewportBuffer.put(1,viewport.y.toInt)
		viewportBuffer.put(2,viewport.w.toInt)
		viewportBuffer.put(3,viewport.h.toInt)
		viewportBuffer.rewind()

		val shaderToRebind = Shader.boundShader
		val textureBlockToRebind = GL.boundTexture(0)
		val fontToRebind = GL.boundTexture(1)


		//		Util.glMatrixBlock{
		Shader.unbind()
		this.drawGL()

		//		}
		shaderToRebind match {
			case None => //Noto.warn("No shader to rebind")
			case Some(shader) => shader.bind()
		}
		GL.bindTexture(0,textureBlockToRebind)
		GL.bindTexture(1,fontToRebind)

		GL.glSetState(GL_DEPTH_TEST,enable = false)
		GL.glSetState(GL_CULL_FACE,enable = false)

		GL.setViewport(startViewport)
	}

	def drawGL()

	def pixelToWorldSpaceLine ( windowPos: Vec3f , modelviewMatrix : ReadMat4x4 , projectionMatrix : ReadMat4x4 ) : (Vec3f,Vec3f) = {
		val wp = windowPos
//		val objCoordinates = BufferUtils.createFloatBuffer(16)
//		GLU.gluUnProject(windowPos.x,windowPos.y,0.0f,modelviewMatrix,projectionMatrix,viewportBuffer,objCoordinates)
		val near = GL.unproject(Vec3f(wp.x,wp.y,0.0f),modelviewMatrix,projectionMatrix,viewport.toRecti)
//		val (nearx,neary,nearz) = (objCoordinates.get,objCoordinates.get,objCoordinates.get)
//		objCoordinates.rewind()
		val far = GL.unproject(Vec3f(wp.x,wp.y,1.0f),modelviewMatrix,projectionMatrix,viewport.toRecti)
//		GLU.gluUnProject(windowPos.x,windowPos.y,1.0f,modelviewMatrix,projectionMatrix,viewportBuffer,objCoordinates)
//		val (farx,fary,farz) = (objCoordinates.get,objCoordinates.get,objCoordinates.get)
//		val res = (Vec3f(nearx,neary,nearz),Vec3f(farx,fary,farz))
//		res
		near -> far
	}

	var lastOffset : ReadVec3f = Vec3f.Zero

	override protected def drawSelf(vbo: AVBO, context: RenderingContext): Unit = {
		super.drawSelf(vbo,context)
		context.newOpenglWidgetIndices :+= (vbo.numIndices,this)
		lastOffset = Vec3f(context.translationPixelsX,context.translationPixelsY + 0.5f,0.0f)
	}
}

object OpenGLWidget {
	object OpenGLRenderer extends WidgetRenderingComponent {

		def draw(widget: Widget, vbo: AVBO, context: RenderingContext, beforeChildren: Boolean){
			widget match {
				case glw : OpenGLWidget => {
					val cbounds = context.boundsStack.head
//					glw.currentBounds = Rectf( context.fromPixelScaleX(cbounds.x) , context.fromPixelScaleY(cbounds.y) , context.fromPixelScaleX(cbounds.w) , context.fromPixelScaleY(cbounds.h) )
					glw.currentBounds = cbounds
					glw.topLevelBounds = context.boundsStack.last
				}
				case _ => Noto.warn("What is a non-opengl widget doing with an opengl renderer?")
			}
		}
	}
}