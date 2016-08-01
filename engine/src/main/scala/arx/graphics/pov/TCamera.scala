package arx.graphics.pov

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/17/15
 * Time: 2:32 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.mat.ReadMat4x4
import arx.core.traits.TSentinel
import arx.core.traits.TSentinelable
import arx.core.units.UnitOfTime
import arx.core.vec.ReadVec3f
import arx.engine.control.event.Event.TEventUser
import arx.graphics.shader.Shader
import scalaxy.loops._

trait TCamera extends TEventUser with TSentinelable {
	def modelviewMatrix : ReadMat4x4
	def projectionMatrix : ReadMat4x4

	def eye : ReadVec3f

	def forward : ReadVec3f
	def ortho : ReadVec3f
	def up : ReadVec3f

	var near = 0.1f
	var viewDistance = 100.0f
	def far = viewDistance * 1.1f
	var fovy = 50.0f

	def update (dt : UnitOfTime)

	def look () = {
		Shader.boundShader match {
			case Some(shader) => {
				shader.setUniform("ModelViewMatrix",modelviewMatrix,tolerateAbsence = true)
				shader.setUniform("ProjectionMatrix",projectionMatrix,tolerateAbsence = true)
			}
			case None => Noto.warn("No shader bound when camera look is applied")
		}
	}
}

object TCamera {
	val Sentinel = new EyeCamera() with TSentinel
}
