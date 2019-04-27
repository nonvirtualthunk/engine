package arx.engine.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 8:19 AM
 */

import java.util.concurrent.atomic.AtomicInteger

import arx.Prelude._
import arx.core.introspection.CopyAssistant

import scalaxy.loops._

trait TAuxData {
	def onAssignedToObject ( entity : THasAuxData[_] ) {}

	/**
	  * Creates a copy-on-write version of itself
	  */
	def copyOnWrite() : this.type = CopyAssistant.copyShallow(this)


	def withData(f : (this.type) => Unit): Unit = {
		f(this)
	}
}


trait TDynamicAuxData extends TAuxData {

}