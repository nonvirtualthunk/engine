package arx.gui2.widgets

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 8/9/15
 * Time: 8:07 AM
 */

import arx.Prelude._
import arx.core.ImplicitModdable._
import arx.core.richer.FloatRange
import arx.core.vec.ReadVec4f
import arx.graphics.helpers.Color
import arx.graphics.AVBO
import arx.graphics.Image
import arx.gui2.Widget
import arx.gui2.WindowingSystem2.RenderingContext
import arx.gui2.rendering.WidgetRenderingComponent
import arx.gui2.widgets.GraphWidget.Dataset
import arx.resource.ResourceManager

class GraphWidget(parentis : Option[Widget], data : () => List[Dataset]) extends DynamicWidget(parentis) {
	import GraphWidget._

	val graphRegion = new Widget(this)
	graphRegion.renderers = List(new GraphWidgetRenderer(data))
	graphRegion.x = 10.0f
	graphRegion.y = 10.0f
	graphRegion.width = () => this.width - 20.0f
	graphRegion.height = () => this.height - 20.0f

	configure[Axis](() => rawDataToChildData(data()),axisToWidget,(l : List[Widget]) => {})

	def axisToWidget (axis : Axis, p : Widget) = {
		val aw = new Widget(p)

		if (axis.horizontal) {
			aw.x = 0.0f
			aw.y = p.clientHeight - 10.0f
			aw.width = p.clientWidth
			aw.height = 10.0f
		} else {
			aw.x = 0.0f
			aw.y = 0.0f
			aw.width = 10.0f
			aw.height = p.clientHeight
		}
		aw.backgroundImage = Image.Sentinel
		
		for (tick <- axis.ticks) {
			val p = (tick - axis.range.lower) / (axis.range.length)
			val (x,y) = axis.horizontal match {
				case true => (graphRegion.x + p * graphRegion.clientWidth) -> 5.0f
				case false => 5.0f -> (graphRegion.y + (1.0f - p) * graphRegion.clientHeight)
			}

			val label = new TextDisplayWidget(decimalFormatter.format(tick),aw)
			label.matchTextDimensions()
			label.backgroundImage = Image.Sentinel
			label.centerX = x
			label.centerY = y
			label.fontSize = 3.0f
//			if (axis.horizontal) { label.centerX = x }
//			else { label.centerY = y }

		}

		aw
	}

	def rawDataToChildData (ds : List[Dataset]) : List[Axis] = {
		val xr = combinedXRange(ds)
		val yr = combinedYRange(ds)
		val hticks = (0 until 7).map(i => xr.lower + xr.length * (i.toFloat / 6.0f)).toList
		var vticks = (0 until 7).map(i => yr.lower + yr.length * (i.toFloat / 6.0f)).toList

		if (yr.contains(0.0f)) {
			vticks ::= 0.0f
		}

		Axis(horizontal = true,hticks,xr) :: Axis(horizontal = false,vticks,yr) :: Nil
	}


}
object GraphWidget {
	case class Axis (horizontal : Boolean, ticks : List[Float], range : FloatRange)

	def xRange (l : List[(Float,Float)]) : FloatRange = l match { case Nil => 0.0f -> 0.0f; case _ => l.unzip._1.min -> l.unzip._1.max }
	def yRange (l : List[(Float,Float)]) : FloatRange = l match { case Nil => 0.0f -> 0.0f; case _ => l.unzip._2.min -> l.unzip._2.max }

	def combinedXRange (ds : Seq[Dataset]) = {
		var xr : FloatRange = 0.0f -> 0.0f
		for (d <- ds) {
			val txr = xRange(d.points)
			xr = txr.lower.min(xr.lower) -> txr.upper.max(xr.upper)
		}
		xr
	}
	def combinedYRange (ds : Seq[Dataset]) = {
		var yr : FloatRange = 0.0f -> 0.0f
		for (d <- ds) {
			val tyr = yRange(d.points)
			yr = tyr.lower.min(yr.lower) -> tyr.upper.max(yr.upper)
		}
		yr
	}

	case class Dataset (points : List[(Float,Float)], color : ReadVec4f)
}


class GraphWidgetRenderer(graphData : () => Seq[Dataset]) extends WidgetRenderingComponent {
	override def draw(widget: Widget, vbo: AVBO, context: RenderingContext, beforeChildren: Boolean): Unit = {
		val rawMargin = 4

		val rx = 0.0f
		val ry = 0.0f
		val rw = roundf(context.toPixelScaleX(widget.clientWidth))
		val rh = roundf(context.toPixelScaleX(widget.clientHeight))

		val bx = rx + rawMargin
		val by = ry + rawMargin
		val bw = rw - rawMargin * 2
		val bh = rh - rawMargin * 2

		val tc = context.textureBlock( ResourceManager.image("default/blankBordered.png") )
		val blankTC = context.textureBlock( ResourceManager.image("default/blank.png") )

		// axes
		drawQuad(vbo,context,rx,ry + rh - 1.0f,rw,rawMargin-2,blankTC,Color.Black)
		drawQuad(vbo,context,rx,ry,rawMargin-2,rh,blankTC,Color.Black)

		val allDatasets = graphData()

		val xr = GraphWidget.combinedXRange(allDatasets)
		val yr = GraphWidget.combinedYRange(allDatasets)

		for (dataset <- allDatasets) {
			val points = dataset.points
			if (points.nonEmpty) {

				for ( (xd,yd) <- points ) {
					val xp = (xd - xr.lower) / xr.length
					val yp = 1.0f - ((yd - yr.lower) / yr.length) // reverse y, we want bottom up

					val x = bx + xp * bw
					val y = by + yp * bh
					val w = 5
					val h = 5
					drawQuad(vbo,context,x-w/2,y-h/2,w,h, tc, dataset.color)
				}
			}
		}
		
	}
}