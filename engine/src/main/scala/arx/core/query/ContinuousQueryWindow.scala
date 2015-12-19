package arx.core.query

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/8/14
 * Time: 7:55 AM
 */

import arx.Prelude._
import scalaxy.loops._

class ContinuousQueryWindow[T <: AnyRef : Manifest] (query : ContinuousQuery[T],pcnt: Float) {
	var iterator : Iterator[Set[T]] = null
	def next = {
		if (query.results.isEmpty) {
			Set()
		} else {
			if (iterator == null || ! iterator.hasNext) {
				val n = (query.results.size * pcnt).max(1).toInt
				iterator = query.results.sliding(n)
			}
			iterator.next()
		}
	}
}