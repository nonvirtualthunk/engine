package arx.graphics.attributeprofiles

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 12/27/15
  * Time: 9:48 AM
  */

import arx.Prelude._
import arx.graphics.AttributeProfile
import org.lwjgl.opengl.GL11._
import scalaxy.loops._
import arx.core.vec._

object LineAttributeProfile extends AttributeProfile(
	List("PrevVertex" ->(3, GL_FLOAT),
		"Vertex" ->(3, GL_FLOAT),
		"NextVertex" ->(3, GL_FLOAT),
		"Orientation" -> (1, GL_FLOAT),
		"TexCoord" ->(2, GL_FLOAT),
		"Color" ->(4, GL_UNSIGNED_BYTE),
		"Thickness" -> (1, GL_FLOAT))) {
	val PrevVertexAttribute = attributesByName("PrevVertex")
	val VertexAttribute = attributesByName("Vertex")
	val NextVertexAttribute = attributesByName("NextVertex")
	val OrientationAttribute = attributesByName("Orientation")
	val TexCoordAttribute = attributesByName("TexCoord")
	val ColorAttribute = attributesByName("Color")
	val ThicknessAttribute = attributesByName("Thickness")

	vertexAttributeIndex = VertexAttribute
	texCoordAttributeIndex = TexCoordAttribute
}

/**
in vec3 PrevVertex;
in vec3 Vertex;
in vec3 NextVertex;
in float orientation;

in vec2 TexCoord;
in vec4 Color;
in float Thickness;
  */