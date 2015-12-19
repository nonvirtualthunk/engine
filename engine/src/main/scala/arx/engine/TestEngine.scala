package arx.engine

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/7/15
 * Time: 7:09 AM
 */

import arx.Prelude._
import arx.core.vec.Vec3f
import arx.graphics.AVBO
import arx.graphics.DynamicVBO
import arx.graphics.Texture
import arx.graphics.attributeprofiles.SimpleAttributeProfile
import arx.resource.ResourceManager
import org.lwjgl.opengl.GL11._
import scalaxy.loops._

object TestEngine extends EngineCore {
	lazy val vbo : AVBO = {
		val ret = new AVBO(SimpleAttributeProfile)
		ret.incrementVertexOffset(4)
		ret.incrementIndexOffset(6)

		val depth = 0.0f
		val size = 10

		ret.setA(SimpleAttributeProfile.VertexAttribute, 0, -size, -size, depth)
		ret.setA(SimpleAttributeProfile.VertexAttribute, 1, size, -size, depth)
		ret.setA(SimpleAttributeProfile.VertexAttribute, 2, size, size, depth)
		ret.setA(SimpleAttributeProfile.VertexAttribute, 3, -size, size, depth)

		ret.setAbf(SimpleAttributeProfile.ColorAttribute, 0, 1.0f,1.0f,1.0f,1.0f)
		ret.setAbf(SimpleAttributeProfile.ColorAttribute, 1, 1.0f,1.0f,1.0f,1.0f)
		ret.setAbf(SimpleAttributeProfile.ColorAttribute, 2, 1.0f,1.0f,1.0f,1.0f)
		ret.setAbf(SimpleAttributeProfile.ColorAttribute, 3, 1.0f,1.0f,1.0f,1.0f)

		ret.setA(SimpleAttributeProfile.TexCoordAttribute, 0, 0.0f, 0.0f)
		ret.setA(SimpleAttributeProfile.TexCoordAttribute, 1, 1.0f, 0.0f)
		ret.setA(SimpleAttributeProfile.TexCoordAttribute, 2, 1.0f, 1.0f)
		ret.setA(SimpleAttributeProfile.TexCoordAttribute, 3, 0.0f, 1.0f)

		ret.setIQuad(0,0)
		ret.state.set(DynamicVBO.Updated)
		ret.lastUpdatedMarker = 1

		ret
	}

	lazy val shader = ResourceManager.shader("shaders/Simple")

	lazy val image = ResourceManager.image("default/defaultium.png")

	lazy val texture = Texture.fromImage(image)

	val modelview = arx.graphics.GL.lookAt(Vec3f(-15.0f,0.0f,-5.0f),Vec3f(-5.0f,0.0f,0.0f),Vec3f(0.0f,1.0f,0.0f))
	val projection = arx.graphics.GL.perspective(50.0f,WIDTH/HEIGHT.toFloat,0.1f,1000.0f)

	//	val modelview = Mat4x4.Identity
	//	val projection = Mat4x4.Identity

	def update(seconds : Float) {}

	def draw(): Unit = {
		arx.graphics.GL.glSetState(GL_CULL_FACE, enable = false)
		arx.graphics.GL.glSetState(GL_DEPTH_TEST, enable = false)
		//		arx.graphics.GL.glSetState(GL_ALPHA_TEST, enable = false)

		shader.bind()
		shader.setUniform("ModelViewMatrix", modelview, tolerateAbsence = true)
		shader.setUniform("ProjectionMatrix", projection, tolerateAbsence = true)

		texture.mipmap = false
		texture.bind()

		vbo.solidifyIfNecessary()
		vbo.bind()
		vbo.drawElements(GL_TRIANGLES)

		//		glBegin(GL_TRIANGLES)
		//		glColor4f(1.0f,0.0f,0.0f,1.0f)
		//
		//		glTexCoord2f(0.0f,0.0f)
		//		glVertex3f(-10.0f,-10.0f,0.0f)
		//		glTexCoord2f(1.0f,0.0f)
		//		glVertex3f(10.0f,-10.0f,0.0f)
		//		glTexCoord2f(0.0f,1.0f)
		//		glVertex3f(10.0f,10.0f,0.0f)
		//
		//		glEnd()
	}
}