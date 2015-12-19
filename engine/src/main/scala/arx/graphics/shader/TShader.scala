package arx.graphics.shader

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/11/13
 * Time: 11:46 AM
 * Created by nonvirtualthunk
 */

import arx.core.mat.ReadMat4x4

trait TShader {
	def bind ()

	def setUniform(uniformName: String, v: ReadMat4x4, tolerateAbsence: Boolean )
}