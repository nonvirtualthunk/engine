package arx.core.mat

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/19/13
 * Time: 1:28 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto

object MatrixFunctions {
	def inverse ( mat : Mat3x3 ) = {
		val c0 = mat.m11*mat.m22 - mat.m12*mat.m21
		val c1 = mat.m12*mat.m20 - mat.m10*mat.m22
		val c2 = mat.m10*mat.m21 - mat.m11*mat.m20

		val determinant = mat.m00*c0 + mat.m01*c1 + mat.m02*c2

		if ( determinant != 0.0f ) {
			val invDet = 1.0f/determinant
			new Mat3x3(
				c0*invDet,
				c1*invDet,
				c2*invDet,

				(mat.m02*mat.m21 - mat.m01*mat.m22)*invDet,
				(mat.m00*mat.m22 - mat.m02*mat.m20)*invDet,
				(mat.m01*mat.m20 - mat.m00*mat.m21)*invDet,

				(mat.m01*mat.m12 - mat.m02*mat.m11)*invDet,
				(mat.m02*mat.m10 - mat.m00*mat.m12)*invDet,
				(mat.m00*mat.m11 - mat.m01*mat.m10)*invDet
			)
		} else {
			Noto.severeError("Inverse taken of 0-determinant matrix")
			Mat3x3.Identity
		}
	}
}