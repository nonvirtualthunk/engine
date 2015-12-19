package arx.core.mathutil

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/26/13
 * Time: 9:58 AM
 */

import arx.Prelude._


import scalaxy.loops._
import scala.util.Random
import arx.application.Noto

class WeightedDistribution[T] ( weights : Array[Float] , values : Array[T] ) {
	val sum = weights.sum

	def rand ( r : Random ) : T = {
		var f = r.nextFloat() * sum
		var i = 0
		while ( i < weights.length ) {
			if ( f < weights(i) ) { return values(i) }
			else {
				f -= weights(i)
				i += 1
			}
		}
		Noto.warn(s"It should not be possible to reach past the end of a weighted distribution, f value is $f")
		values.last
	}
	def rand () : T = {
		rand( arx.Prelude.random )
	}
}


object WeightedDistribution {
	def apply[T : Manifest] ( distribution : (Float,T) * ) : WeightedDistribution[T] = {
		val (weights,values) = distribution.unzip
		new WeightedDistribution[T](weights.toArray,values.toArray)
	}

	def apply[T : Manifest] ( distribution : List[(Float,T)] ) : WeightedDistribution[T] = {
		val (weights,values) = distribution.unzip
		new WeightedDistribution[T](weights.toArray,values.toArray)
	}
}
