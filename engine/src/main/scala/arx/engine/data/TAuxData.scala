package arx.engine.data

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 8:19 AM
 */

import java.util.concurrent.atomic.AtomicInteger

import arx.Prelude._
import scalaxy.loops._

trait TAuxData {
	def onAssignedToObject ( entity : THasAuxData[_] ) {}
}


class AuxDataObject[U <: TAuxData : Manifest] {
	private final val internalId =  AuxDataObject.IdCounter.getAndIncrement
	val clazz : Class[U] = manifest[U].runtimeClass.asInstanceOf[Class[U]]

	def id = internalId
}

object AuxDataObject {
	protected final val IdCounter = new AtomicInteger(1)
}