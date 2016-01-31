package arx.engine

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/19/15
 * Time: 5:13 PM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.Dependency
import arx.core.TDependable
import arx.core.datastructures.UpdateThread
import arx.core.traits.TUpdateable

abstract class EnginePiece[Component <: TDependable with TUpdateable : Manifest] {
	var parallelism = 4
	var components = List[Component]()

	protected var initialized = false

	def createUpdateThread(i: Int): UpdateThread = {
		new UpdateThread(0.01.seconds) {
			// divide up the components such that each graphics thread has its own share
			val localComponents = components.zipWithIndex.filter {
				case(comp, index) => index % parallelism == i
			}.unzip.left

			override def update(): Unit = {
				localComponents.foreach(c => c.updateSelf(rawInterval.seconds))
			}
		}
	}

	protected lazy val updateThreads = fillList(parallelism)(i => createUpdateThread(i))

	def addComponent[T <: Component: Manifest] = {
		components ::= instantiateComponent(List(manifest[T].runtimeClass)).asInstanceOf[Component]
	}

	protected def instantiateComponent(l : List[Class[_]]) : AnyRef

	def update (deltaSeconds : Float): Unit = {
		if (!initialized) {
			initialize()
		}

		updateThreads.foreach(t => t.timePassed(deltaSeconds.seconds))
	}

	def updateSerial (deltaSeconds : Float): Unit = {
		components.foreach(c => c.updateSelf(deltaSeconds.seconds))
	}

	def initialize(): Unit = {
		val resolved = Dependency.resolve(components, components, instantiateComponent)
		val allComponents = resolved.ofType[Component]
		val nonComponents = resolved.notOfType[Component]

		components = allComponents
		nonComponents.foreach(nc => Noto.info(s"Instantiated non-component for engine: $nc"))
		initialized = true
	}
}
