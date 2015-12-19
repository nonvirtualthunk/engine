package arx.core.mathutil

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/1/13
 * Time: 1:34 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.mat.Basis
import arx.core.vec.{Vec3f, ReadVec2f, Vec2f, ReadVec3f}


import arx.core.mat.Mat3x4

object Trig {
	def vectorToPitchAndYaw ( v : ReadVec3f ) = {
		val yaw = math.atan2(v.y,v.x)
		val pitch = math.atan2(-v.z,math.sqrt(v.x*v.x + v.y*v.y))

		Vec2f(yaw.toFloat,pitch.toFloat)
	}

	def pitchAndYawToVector ( angles : ReadVec2f ) = {
		val transform = (Mat3x4 rotateY angles.y) rotateZ(angles.x)
		transform transformPoint Vec3f.UnitX
	}

	def pitchAndYawToBasis ( angles : ReadVec2f ) = {
		val transform = (Mat3x4 rotateY angles.y) rotateZ(angles.x)
		val forward = transform transformVector Vec3f.UnitX
		val up = transform transformVector Vec3f.UnitZ
		val ortho = up cross forward
		new Basis(forward,ortho,up)
	}
}

