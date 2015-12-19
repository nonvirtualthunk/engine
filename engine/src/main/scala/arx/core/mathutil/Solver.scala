package arx.core.mathutil

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 10:22 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._


import arx.application.Noto

object Solver {
	def solveQuadratic ( a : Float , b: Float , c: Float ) = {
		val underRadical = (b*b) - 4.0f * a * c
		if ( underRadical < 0.0f ) { Nil }
		else if ( underRadical == 0.0f ) {
			List((b * -1.0f) / (2.0f * a))
		} else {
			val radical = sqrtf(underRadical)
			List((b * -1.0f + radical) / (2.0f * a),(b * -1.0f - radical) / (2.0f * a))
		}
	}
}
