package arx.engine.graphics.components

/**
  * TODO: Add javadoc
  */

import java.util.concurrent.atomic.AtomicLong

import arx.Prelude._
import arx.application.Noto
import arx.core.datastructures.Watcher
import arx.core.datastructures.Watcher2
import arx.core.datastructures.Watcher3
import arx.core.introspection.ReflectionAssistant
import arx.core.math.Rectf
import arx.core.units.UnitOfTime
import arx.core.vec.Vec2f
import arx.core.vec.Vec2i
import arx.core.vec.Vec3i
import arx.core.vec.Vec4f
import arx.engine.control.components.windowing.Widget
import arx.engine.control.components.windowing.widgets.Desktop
import arx.engine.control.components.windowing.widgets.DimensionExpression
import arx.engine.control.components.windowing.widgets.PositionExpression
import arx.engine.control.components.windowing.widgets.TopLeft
import arx.engine.control.components.windowing.widgets.data.DrawingData
import arx.engine.graphics.GraphicsEngine
import arx.engine.graphics.components.WindowingGraphicsComponent.WidgetWatchers
import arx.engine.graphics.components.windowing.Constraints
import arx.engine.graphics.components.windowing.Solver
import arx.engine.graphics.components.windowing.WQuad
import arx.engine.graphics.components.windowing.WindowingRenderer
import arx.engine.graphics.data.WindowingGraphicsData
import arx.graphics.AVBO
import arx.graphics.GL
import arx.graphics.TextureBlock
import arx.graphics.VBO
import arx.graphics.helpers.Color
import arx.gui2.rendering.WindowingSystemAttributeProfile2
import arx.gui2.rendering.WindowingSystemAttributeProfile2._
import arx.resource.ResourceManager
import org.lwjgl.opengl.GL11
import org.pybee.cassowary.Constraint
import org.pybee.cassowary.Expression
import org.pybee.cassowary.SimplexSolver
import org.pybee.cassowary.Strength
import org.pybee.cassowary.Variable

import scala.collection.mutable
import scala.language.postfixOps
import scalaxy.loops._

class WindowingGraphicsComponent(graphicsEngine: GraphicsEngine) extends GraphicsComponent(graphicsEngine) {
	lazy val shader = ResourceManager.shader("shaders/windowing/main")

	val WD = graphics[WindowingGraphicsData]

	def vbo = WD.vbo
	def textureBlock = WD.textureBlock

	val renderers = ReflectionAssistant.allSubTypesOf[WindowingRenderer]
		.map(c => ReflectionAssistant.instantiate(c, WD))

	val watchers = new mutable.HashMap[Widget, WidgetWatchers]()
	val solver = new Solver

	val updateRevision = new AtomicLong(1L)
	val solidifiedRevision = new AtomicLong(0L)

	var renderedViewport = GL.viewport

	drawOrder = DrawPriority.Final

	def createWatchers(widget: Widget) = WidgetWatchers(Watcher(widget.position), Watcher(widget.dimensions))

	override def draw(): Unit = {
		renderedViewport = GL.viewport

		shader.bind()
		shader.setUniform("mainTexture",0)

		WD.pov.look()

		textureBlock.bind(0)

		GL.glSetState(GL11.GL_DEPTH_TEST, false)

		val cur = updateRevision.get()
		if (vbo.solidifyIfNecessary()) {
			solidifiedRevision.set(cur)
			// if we have been modified again since the point at which we started solidifying, mark for re-drawing
			if (solidifiedRevision.get() < updateRevision.get()) {
				vbo.state.set(VBO.Dirty)
			}
		}
		vbo.drawElements(GL11.GL_TRIANGLES)

		GL.glSetState(GL11.GL_DEPTH_TEST, true)
	}

	val viewportWatcher = new Watcher(renderedViewport)
	override protected def update(dt: UnitOfTime): Unit = {
		var anyChanged = false
		if (viewportWatcher.hasChanged) {
			WD.desktop.width = DimensionExpression.Constant(GL.viewport.width)
			WD.desktop.height = DimensionExpression.Constant(GL.viewport.height)
			anyChanged = true
		}

		anyChanged ||= updateWidgetConstraints(WD.desktop)

//		solver.resolve()

		anyChanged ||= solver.updateDynamicExpressions()
		if (anyChanged) {
			updateRevision.incrementAndGet()
			if (!vbo.changeState(VBO.Updated, VBO.Dirty) && !vbo.changeState(VBO.Clean, VBO.Dirty)) {
				Noto.info(s"Could not change to dirty on update, currently is : ${vbo.state.get()}")
			}

			updateResolvedWidgetVariables(WD.desktop)
		}

//		solver.solve(anyChanged)

		if (vbo.changeState(VBO.Dirty, VBO.Updating)) {
			vbo.clear()
			// could if(anyChanged) here
			updateWindowingDrawData(WD.desktop, Rectf(0, 0, GL.viewport.w, GL.viewport.h))
			vbo.state.set(VBO.Updated)
		}
	}

	val colors = Color(255,255,255,255) :: Color(255,0,0,255) :: Color(0,255,0,255) :: Color(255,255,0,255) :: Nil

	def updateWindowingDrawData(w: Widget, bounds: Rectf): Unit = {
		val relPos = w.drawing.relativePosition
//		Noto.info(s"Widget ${w.identifier} of class ${w.getClass.getSimpleName} bounds $bounds and relPos $relPos")
		Noto.indentation += 1

		def renderQuad(quad : WQuad): Unit = {
			val ii = vbo.incrementIndexOffset(6)
			val vi = vbo.incrementVertexOffset(4)

			vbo.setA(V, vi + 0, bounds.x + relPos.x + quad.rect.minX, bounds.y + relPos.y + quad.rect.minY)
			vbo.setA(V, vi + 1, bounds.x + relPos.x + quad.rect.minX, bounds.y + relPos.y + quad.rect.maxY)
			vbo.setA(V, vi + 2, bounds.x + relPos.x + quad.rect.maxX, bounds.y + relPos.y + quad.rect.maxY)
			vbo.setA(V, vi + 3, bounds.x + relPos.x + quad.rect.maxX, bounds.y + relPos.y + quad.rect.minY)

			val imgRect = textureBlock.getOrElseUpdateRectFor(quad.image)
			val tpos = imgRect.xy  + quad.subRect.xy * imgRect.dimensions
			val tdim = imgRect.dimensions * quad.subRect.dimensions
			val tcs = quad.texCoords match {
				case Some(rawTcs) => rawTcs
				case None => Array(Vec2f(tpos.x,tpos.y),
					Vec2f(tpos.x + tdim.x,tpos.y),
					Vec2f(tpos.x + tdim.x,tpos.y + tdim.y),
					Vec2f(tpos.x,tpos.y + tdim.y))
			}

			val toff = 3 + quad.rotation / 90
			for (q <- 0 until 4 optimized) {
				vbo.setA(C, vi + q, quad.color)
				vbo.setA(TC, vi + q, tcs((q + toff) % 4))
				vbo.setA(B, vi + q, bounds.x, bounds.y, bounds.x + bounds.w, bounds.y + bounds.h)
			}

//			Noto.info(s"Drawing quad: $quad")
			vbo.setIQuad(ii, vi)
		}

		renderers.flatMap(_.render(w, beforeChildren = true)).foreach(renderQuad)

//		renderQuad(WQuad(Rectf(-relPos.x,-relPos.y,bounds.width,bounds.height), "default/blank_bordered.png", colors(Noto.indentation)))


		val pos = w.drawing.absolutePosition + Vec3i(w.clientOffset, 0)
		val size = w.clientDim
		val newBounds = bounds.intersect(Rectf(pos.x, pos.y, size.x, size.y))
		for (child <- w.children) {
			updateWindowingDrawData(child, newBounds)
		}

		renderers.flatMap(_.render(w, beforeChildren = false)).foreach(renderQuad)

		Noto.indentation -= 1
	}


	override protected def initialize(): Unit = {
		WD.desktop.x = PositionExpression.Constant(0)
		WD.desktop.y = PositionExpression.Constant(0)
		WD.desktop.z = PositionExpression.Constant(0)

		WD.desktop.width = DimensionExpression.Constant(GL.viewport.width)
		WD.desktop.height = DimensionExpression.Constant(GL.viewport.height)
	}

	val standardSize = Vec2i(10,10)
	def calculateIntrinsicDimFor(w : Widget) = {
		renderers.findFirstWith(r => r.intrinsicSize(w)) match {
			case Some((renderer,size)) => size
			case None => standardSize
		}
	}

	def calculateDecorationBorderSize(w : Widget) = {
		renderers.findFirstWith(r => r.decorationBorderSize(w)) match {
			case Some((renderer,size)) => size
			case None => Vec2i.Zero
		}
	}

	val largeConstExpr = new Expression(100000)

	implicit class EnrichedWidget(w: Widget) {
		def clientDim = {
			val d = w.drawing
			d.effectiveDimensions - clientOffset * 2
		}

		def clientOffset = {
			val d = w.drawing
			d.interiorPadding + d.decorationBorderSize
		}
	}

	def updateWidgetConstraints(widget: Widget): Boolean = {
		val watch = watchers.getOrElseUpdate(widget, createWatchers(widget))

		val ret = widget.isModified ||
			(if (watch.first || watch.anyChanged) {
//					for (axis <- 0 until 3 optimized) {
//						if (watch.first || watch.posWatcher.hasChanged(axis)) updatePos(widget, axis)
//					}
//					for (axis <- 0 until 2 optimized) {
//						if (watch.first || watch.dimWatcher.hasChanged(axis)) updateDim(widget, axis)
//					}

					watch.first = false
					true
				} else {
					false
				})

		val anyChildModified = widget.children.exists(updateWidgetConstraints)
		ret || anyChildModified
	}

	def resolveEffectiveDimensions(widget : Widget) = {
		val ret = new Vec2i(0,0)
		for (axis <- 0 until 2 optimized) {
			ret(axis) = (widget.dimensions(axis) match {
				case DimensionExpression.Constant(constValue) =>
					constValue
				case DimensionExpression.Proportional(proportion) =>
					widget.parent.clientDim(axis) * proportion
				case DimensionExpression.Relative(delta) =>
					widget.parent.clientDim(axis) + delta
				case DimensionExpression.Intrinsic =>
					calculateIntrinsicDimFor(widget)(axis)
				// do nothing here, in this case the desired width is determined by the thing itself,
				// a button might suggest that it be given a width at least sufficient to display its
				// text, for example, and other widgets might have intrinsic minimum sizes
			}).round
		}
		ret
	}

	def resolveRelativePosition(widget : Widget) = {
		val ret = new Vec3i(0,0,0)
		for (axis <- 0 until 3 optimized) {
			ret(axis) = (widget.position(axis) match {
				case PositionExpression.Constant(constValue, relativeTo) =>
					if (relativeTo == TopLeft) {
						constValue
					} else {
						widget.parent.clientDim(axis) - widget.drawing.effectiveDimensions(axis) - constValue
					}
				case PositionExpression.Proportional(p, relativeTo) =>
					val offsetExpr = widget.parent.clientDim(axis) * p
					if (relativeTo == TopLeft) {
						offsetExpr
					} else {
						widget.parent.clientDim(axis) - widget.drawing.effectiveDimensions(axis) - offsetExpr
					}
				case PositionExpression.Flow =>
					0
				// do nothing there, this is considered unconstrained, will be filled in
				// more intelligently by the remaining layout engine...unless we deal with it
				// here, which we could
			}).round
		}
		ret
	}

	def updateResolvedWidgetVariables(w: Widget): Unit = {
		val DD = w[DrawingData]
		DD.effectiveDimensions = resolveEffectiveDimensions(w)
		DD.decorationBorderSize = calculateDecorationBorderSize(w)
		w match {
			case d : Desktop =>
			case _ =>
				DD.relativePosition = resolveRelativePosition(w)
				DD.absolutePosition = w.parent.drawing.absolutePosition + Vec3i(w.parent.clientOffset,0) + DD.relativePosition
		}

		w.children.foreach(updateResolvedWidgetVariables)
	}
}

object WindowingGraphicsComponent {
	case class WidgetWatchers(posWatcher: Watcher3[PositionExpression],
									  dimWatcher: Watcher2[DimensionExpression]) {
		var first = true

		def peekAnyChanged = posWatcher.peekChanged || dimWatcher.peekChanged
		def anyChanged = posWatcher.hasChanged || dimWatcher.hasChanged
	}
}