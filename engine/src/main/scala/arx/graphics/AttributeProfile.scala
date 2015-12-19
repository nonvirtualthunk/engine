package arx.graphics

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/19/13
 * Time: 6:30 PM
 * Created by nonvirtualthunk
 */

import arx.application.Noto
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL30

class AttributeProfile(m : List[(String,(Int,Int))]) {
	var attributes = new Array[AttribInfo](m.size)
	var _attributesByName : Map[String,Int] = Map[String,Int]()
	def attributesByName( str : String ) : Int = _attributesByName(str)
	var byteStride = 0


	private var i = 0
	for ( (name,(size,dataType)) <- m ) {
		attributes(i) = AttribInfo(size,dataType,byteOffset = byteStride,name = name,normalize = true)
		byteStride += size * (dataType match {
			case GL_BYTE => 1
			case GL_FLOAT => 4
			case GL_INT => 4
			case GL_SHORT => 2
			case GL_UNSIGNED_BYTE => 1
			case GL_UNSIGNED_SHORT => 2
			case GL_UNSIGNED_INT => 4
			case GL30.GL_HALF_FLOAT => 2
			case _ => throw new IllegalStateException("Invalid data type for attribute : " + dataType)
		})
		_attributesByName += name -> i
	i += 1}
	if ( byteStride % 4 != 0 ) { Noto.error("Having a non 4-byte aligned byte stride in an attribute profile is unwise") }

	var vertexAttributeIndex = 0
	var texCoordAttributeIndex = 1
}

object AttributeProfile {
	def apply ( m : (String,(Int,Int)) * ) = new AttributeProfile(m.toList)
}

object DefaultAttributeProfile extends AttributeProfile( List("vertex" -> (3,GL_FLOAT)) )