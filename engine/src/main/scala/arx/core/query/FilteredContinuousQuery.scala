package arx.core.query

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 11/30/14
 * Time: 4:03 PM
 */

import arx.Prelude._
import scalaxy.loops._

class FilteredContinuousQuery[T <: AnyRef : Manifest](filter : (AnyRef) => Option[T], wrapped : ContinuousQuery[T]) extends ContinuousQuery[T](filter) { self =>
	wrapped.withListener(new ContinuousQueryListener[T] {
		override def queryResultAdded(t: T): Unit = self.add(t)
		override def queryResultRemoved(t: T): Unit = self.remove(t)
	},fireOnExistingResults = true)
}
