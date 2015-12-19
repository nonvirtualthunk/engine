package arx.engine.graphics

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/17/15
 * Time: 9:58 AM
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.Dependency
import arx.core.datastructures.UpdateThread
import arx.engine.graphics.components.GraphicsComponent
import arx.engine.world.World
import arx.graphics.pov.EyeCamera
import arx.graphics.pov.TCamera

class GraphicsEngine(world : World) {
	var parallelism = 4
	var graphicsComponents = List[GraphicsComponent]()
	var pov : TCamera = new EyeCamera()

	protected var initialized = false
	protected lazy val graphicsThreads = fillList(parallelism)(i => new UpdateThread(0.01.seconds) {
		// divide up the components such that each graphics thread has its own share
		val localComponents = graphicsComponents.zipWithIndex.filter {
			case(comp, index) => index % parallelism == i
		}.unzip.left

		override def update(): Unit = {
			localComponents.foreach(c => c.update(rawInterval.seconds))
		}
	})

	def addComponent[T <: GraphicsComponent : Manifest] = {
		graphicsComponents ::= instantiateComponent(List(manifest[T].runtimeClass)).asInstanceOf[GraphicsComponent]
	}

	protected def instantiateComponent(l : List[Class[_]]) : AnyRef = {
		l.find(c => try { c.getConstructor(classOf[GraphicsEngine], classOf[World]) != null } catch { case e : Exception => false }) match {
			case Some(clazz) => {
				val constructor = clazz.getConstructor(classOf[GraphicsEngine], classOf[World]);
				constructor.newInstance(this, world).asInstanceOf[GraphicsComponent]
			}
			case None => throw new IllegalStateException(s"Could not instantiate graphics component of possible types $l")
		}
	}

	def update (deltaSeconds : Float): Unit = {
		if (!initialized) {
			initialize()
		}

		graphicsThreads.foreach(t => t.timePassed(deltaSeconds.seconds))
	}

	def initialize(): Unit = {
		val resolved = Dependency.resolve(graphicsComponents, graphicsComponents, instantiateComponent)
		val allComponents = resolved.ofType[GraphicsComponent]
		val nonComponents = resolved.notOfType[GraphicsComponent]

		graphicsComponents = allComponents
		nonComponents.foreach(nc => Noto.info(s"Instantiated non-component for graphics engine: $nc"))
	}

	def draw(): Unit = {
		graphicsComponents = graphicsComponents.sortBy(_.drawPriority * -1)
		graphicsComponents.foreach(g => g.draw())
	}
}



