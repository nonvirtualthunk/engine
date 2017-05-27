package arx.engine

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 12/19/15
  * Time: 5:13 PM
  */

import java.util.concurrent.locks.LockSupport

import arx.Prelude
import arx.Prelude._
import arx.application.Noto
import arx.core.Dependency
import arx.core.datastructures.Killable
import arx.core.datastructures.KillableThread
import arx.engine.traits.EngineComponent

import scalaxy.loops.rangeExtensions

abstract class EnginePiece[Component <: EngineComponent : Manifest] {
	var parallelism = 3
	var components = List[Component]()
	protected var componentClasses = List[Class[_ <: Component]]()
	var updateThreads = List[KillableThread]()

	protected var initialized = false

	def currentTime() = Prelude.curTime()

	def createUpdateThread(i: Int): KillableThread = {
		new KillableThread(Killable.GameLevel) {
			override def whileRunningDo(): Unit = {
				var anyUpdates = false

				for (component <- components) {
					val t = currentTime()
					val delta = t - component.lastUpdated
					if (delta > component.updateInterval) {
						if (component.updateInProgress.compareAndSet(false, true)) {
							component.updateSelf(delta)
							component.lastUpdated = t
							component.updateInProgress.set(false)
							anyUpdates = true
						}
					}
				}

				if (!anyUpdates) {
					// wait for 3 ms before having another look
					LockSupport.parkNanos(3000000)
				}
			}
		}
	}

	def addComponent[T <: Component : Manifest] = {
		if (!initialized) {
//			components ::= instantiateComponent(List(manifest[T].runtimeClass)).asInstanceOf[Component]
			componentClasses ::= manifest[T].runtimeClass.asInstanceOf[Class[T]]
		} else {
			Noto.severeError("Cannot add component after initialization")
		}
	}

	protected def instantiateComponent(l: List[Class[_]]): AnyRef

	def update(deltaSeconds: Float): Unit = {
		if (!initialized) {
			Noto.severeError("Attempted to update engine piece without initializing first")
		}
	}

	def updateSerial(deltaSeconds: Float, nSteps: Int = 1): Unit = {
		if (!initialized) {
			Noto.severeError("Attempted to update engine piece without initializing first")
		}

		for (n <- 0 until nSteps optimized) {
			components.foreach(c => c.updateSelf(deltaSeconds.seconds))
		}
	}

	def shutdown(): Unit = {
		updateThreads.foreach(t => t.kill())
	}

	def serialMode(): this.type = {
		updateThreads.foreach(t => t.kill())
		this
	}



	def registerComponent(comp : EngineComponent): Unit = {
		if (! comp.isInstanceOf[Component]) {
			Noto.severeError(s"Attempting to add innappropriate object as component, ${comp.getClass.getSimpleName} to ${this.getClass.getSimpleName}")
		} else {
			components ::= comp.asInstanceOf[Component]
		}
	}

//	/**
//	  * initialize a component that has come in after initialization
//	  */
//	protected def initializeComponent[T <: Component : Manifest]() = {
//		val comp = instantiateComponent(List(manifest[T].runtimeClass)).asInstanceOf[Component]
//		components ::= comp
//		val resolved = Dependency.resolve(components, components, instantiateComponent)
//		components = resolved.ofType[Component]
//		comp.updateSelf(0.01.seconds)
//	}

	def resolveComponents(context : List[Any]) = {
		// components automatically register themselves to their engine piece, so we can just inject
		val (comps, newContext) = Dependency.resolveByInjection(componentClasses, this :: context)
		newContext
//		//		val resolved = Dependency.resolve(components, components ::: context, instantiateComponent)
//		val allComponents = resolved.ofType[Component]
//		val nonComponents = resolved.notOfType[Component]
//
//		components = allComponents
//		nonComponents.foreach(nc => Noto.info(s"Instantiated non-component for engine: $nc"))
	}

	def initialize(serial: Boolean): Unit = {

		// Do one synchronous update of every game component, we want to ensure they have a chance to do any necessary
		// initialization
		// todo: should this not be parallel then?
		components.par.foreach(_.updateSelf(0.01.seconds))
		initialized = true

		if (!serial) {
			updateThreads = fillList(parallelism)(i => createUpdateThread(i))
			updateThreads.foreach(t => t.start())
		}
	}
}
