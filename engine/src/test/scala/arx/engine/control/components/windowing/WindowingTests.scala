package arx.engine.control.components.windowing

/**
  * TODO: Add javadoc
  */

import arx.Prelude._
import arx.core.vec.Vec2i
import arx.core.vec.Vec3i
import arx.engine.advanced.Engine
import arx.engine.control.ControlEngine
import arx.engine.control.components.windowing.widgets.BottomRight
import arx.engine.control.components.windowing.widgets.DimensionExpression
import arx.engine.control.components.windowing.widgets.PositionExpression
import arx.engine.control.components.windowing.widgets.TopLeft
import arx.engine.control.data.ControlWorld
import arx.engine.control.data.WindowingData
import arx.engine.event.EventBus
import arx.engine.game.GameEngine
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.WindowingGraphicsComponent
import arx.engine.graphics.data.GraphicsWorld
import arx.engine.world.World
import org.scalatest.FlatSpec

import scalaxy.loops._

abstract class EngineTestFlatSpec extends FlatSpec {
	val self = this

	def setUpEngine(engine : Engine)

	val engine = new Engine {
		override def setUpEngine(): Unit = {
			self.setUpEngine(this)
		}
	}
}

abstract class MinimalEngineTestFlatSpec extends FlatSpec {
	def fixture = new {
		val world = new World
		val graphicsWorld = new GraphicsWorld
		val controlWorld = new ControlWorld
		val gameEventBus = new EventBus
		val graphicsEventBus = new EventBus
		val controlEventBus = new EventBus
		val gameEngine = new GameEngine(world, gameEventBus)
		val graphicsEngine = new GraphicsEngine(world, graphicsWorld, graphicsEventBus, gameEventBus)
		val controlEngine = new ControlEngine(world, graphicsWorld, controlWorld, controlEventBus, graphicsEventBus, gameEventBus)
	}

}

class WindowingTests extends MinimalEngineTestFlatSpec {



	"a widget set to an absolute position" should "have that position" in {
		val f = fixture
		import f._

		graphicsEngine.addComponent[WindowingGraphicsComponent]
		controlEngine.addComponent[WindowingControlComponent]

		val CWD = controlWorld[WindowingData]
		val w = new Widget(CWD.desktop)
		w.x = PositionExpression.Constant(100)
		w.y = PositionExpression.Constant(50)
		w.z = PositionExpression.Constant(10)

		controlEngine.updateSerial(1)
		graphicsEngine.updateSerial(1)

		assert(w.drawing.relativePosition == Vec3i(100,50,10))
		assert(w.drawing.absolutePosition == Vec3i(100,50,10))
	}

	"a widget set to a position relative to the right side" should "be the appropriate distance from that side" in {
		val f = fixture
		import f._

		graphicsEngine.addComponent[WindowingGraphicsComponent]
		controlEngine.addComponent[WindowingControlComponent]

		val CWD = controlWorld[WindowingData]

		val p = new Widget(CWD.desktop)
		p.x = PositionExpression.Constant(20, TopLeft)
		p.y = PositionExpression.Constant(20, TopLeft)
		p.z = PositionExpression.Constant(20)

		p.width = DimensionExpression.Constant(500)
		p.height = DimensionExpression.Constant(500)

		val w = new Widget(p)
		w.x = PositionExpression.Constant(100, BottomRight)
		w.y = PositionExpression.Constant(50, BottomRight)
		w.z = PositionExpression.Constant(10)

		w.width = DimensionExpression.Constant(100)
		w.height = DimensionExpression.Constant(100)

		controlEngine.updateSerial(1)
		graphicsEngine.updateSerial(1)

		assert(w.drawing.relativePosition == Vec3i(300,350,10))
		assert(w.drawing.absolutePosition == Vec3i(320,370,30))

		// change the padding setting and then step forward
		p.drawing.interiorPadding = Vec2i(10,20)

		controlEngine.updateSerial(1)
		graphicsEngine.updateSerial(1)

		// ensure that the subwidget is now adjusted
		assert(w.drawing.relativePosition == Vec3i(280,310,10))
		assert(w.drawing.absolutePosition == Vec3i(300,330,30))
	}


	"a widget set to a proportional value" should "be the correct percentage of the parent" in {
		val f = fixture
		import f._

		graphicsEngine.addComponent[WindowingGraphicsComponent]
		controlEngine.addComponent[WindowingControlComponent]

		val CWD = controlWorld[WindowingData]

		val p = new Widget(CWD.desktop)
		p.x = PositionExpression.Proportional(0.5f, TopLeft)
		p.y = PositionExpression.Proportional(0.1f, BottomRight)

		p.width = DimensionExpression.Proportional(0.2f)
		p.height = DimensionExpression.Proportional(0.2f)

		val w = new Widget(p)
		w.x = PositionExpression.Proportional(0.5f, BottomRight)
		w.y = PositionExpression.Proportional(0.5f, BottomRight)
		w.z = PositionExpression.Constant(10)

		w.width = DimensionExpression.Constant(20)
		w.height = DimensionExpression.Constant(20)

		controlEngine.updateSerial(1)
		graphicsEngine.updateSerial(1)

		val fullH = CWD.desktop.drawing.effectiveDimensions.y
		assert(p.drawing.relativePosition == Vec3i(CWD.desktop.drawing.effectiveDimensions.x / 2,
			fullH - (fullH * 0.1f).toInt - (fullH * 0.2f).toInt,
			0))

//		// Pick up here, these haven't been updated to what they should be
//		assert(w.drawing.relativePosition == Vec3i(300,350,10))
//		assert(w.drawing.absolutePosition == Vec3i(320,370,30))
//
//		// change the padding setting and then step forward
//		p.drawing.interiorPadding = Vec2i(10,20)
//
//		controlEngine.updateSerial(1)
//		graphicsEngine.updateSerial(1)
//
//		// ensure that the subwidget is now adjusted
//		assert(w.drawing.relativePosition == Vec3i(280,310,10))
//		assert(w.drawing.absolutePosition == Vec3i(300,330,30))
	}
}
