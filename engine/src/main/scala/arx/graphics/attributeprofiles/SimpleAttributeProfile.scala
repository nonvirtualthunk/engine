package arx.graphics.attributeprofiles

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/19/13
 * Time: 12:04 PM
 * Created by nonvirtualthunk
 */

import arx.graphics.AttributeProfile
import org.lwjgl.opengl.GL11._

object SimpleAttributeProfile extends AttributeProfile(List("vertex" -> (3,GL_FLOAT),"texCoord" -> (2,GL_FLOAT),"color" -> (4,GL_UNSIGNED_BYTE))) {
	val VertexAttribute = attributesByName("vertex")
	val TexCoordAttribute = attributesByName("texCoord")
	val ColorAttribute = attributesByName("color")

	vertexAttributeIndex = VertexAttribute
	texCoordAttributeIndex = TexCoordAttribute
}