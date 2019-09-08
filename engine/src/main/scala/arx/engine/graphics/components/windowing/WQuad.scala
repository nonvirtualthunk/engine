package arx.engine.graphics.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.math.Rectf
import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec4f
import arx.graphics.TToImage

import scalaxy.loops._

// windowing quad
case class WQuad(rect : Rectf,
					  image : TToImage,
					  color : ReadVec4f,
					  flipX : Boolean = false,
					  flipY : Boolean = false,
					  rotation : Int = 0,
					  subRect : Rectf = WQuad.StandardRect,
					  texCoords : Option[Array[ReadVec2f]] = None) {

}

object WQuad {
	val StandardRect = Rectf(0.0f,0.0f,1.0f,1.0f);
}