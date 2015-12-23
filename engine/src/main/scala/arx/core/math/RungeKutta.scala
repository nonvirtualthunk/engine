package arx.core.math

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 12/22/15
  * Time: 9:25 AM
  */

import arx.Prelude._
import scalaxy.loops._

object RungeKutta {
	def approximate(initialState: Array[Float],
						 h: Float,
						 calcDeriv: Array[Float] => Array[Float],
						 nextState: Array[Float]) {

		val n = initialState.length

		var f = calcDeriv(initialState)
		for (i <- 0 until n optimized) {
			val k = h * f(i)
			f(i) = initialState(i) + k * 0.5f
			nextState(i) = k // k1, we do = rather than += here to replace whatever the original contents were
		}
		f = calcDeriv(f)
		for (i <- 0 until n optimized) {
			val k = h * f(i)
			f(i) = initialState(i) + k * 0.5f
			nextState(i) += k * 2.0f // k2
		}
		f = calcDeriv(f)
		for (i <- 0 until n optimized) {
			val k = h * f(i)
			f(i) = initialState(i) + k
			nextState(i) += k * 2.0f // k3
		}
		f = calcDeriv(f)
		for (i <- 0 until n optimized) {
			val k = h * f(i)
			nextState(i) += k
		}

		for (i <- 0 until n optimized) {
			nextState(i) = initialState(i) + (nextState(i) / 6.0f)
		}

		//		for(i <- 0 until n optimized) {
		//			nextState(i) = initialState(i) + (k1(i) + 2.0f * k2(i) + 2.0f * k3(i) + k4(i)) / 6.0f
		//		}
	}


	def approximate(initialState: IndexedSeq[Float],
						 h: Float,
						 steps: Int,
						 calcDeriv: Array[Float] => Array[Float],
						 onStepCalculated: (Int, Array[Float]) => Unit) {

		var curState = initialState.toArray
		var nextState = Array.ofDim[Float](curState.length)

		for (i <- 0 to steps optimized) {
			onStepCalculated(i, curState)

			approximate(curState, h, calcDeriv, nextState)

			val swap = curState
			curState = nextState
			nextState = swap
		}
	}

	def approximateWhile(initialState: IndexedSeq[Float],
						 h: Float,
						 steps: Int,
						 calcDeriv: Array[Float] => Array[Float],
						 onStepCalculated: (Int, Array[Float]) => Boolean) {

		var curState = initialState.toArray
		var nextState = Array.ofDim[Float](curState.length)

		var i = 0
		while (i <= steps) {
			if (!onStepCalculated(i, curState)) {
				i = steps+1
			} else {
				approximate(curState, h, calcDeriv, nextState)

				val swap = curState
				curState = nextState
				nextState = swap

				i += 1
			}
		}
	}
}
