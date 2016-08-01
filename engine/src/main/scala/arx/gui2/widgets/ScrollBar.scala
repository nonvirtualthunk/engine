package arx.gui2.widgets

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/4/14
 * Time: 9:04 AM
 */

import arx.Prelude._
import arx.core.ImplicitModdable._
import arx.core.traits.TNonDiscoverable
import arx.core.vec.Cardinals._
import arx.core.CachedFloat
import arx.core.Moddable
import arx.engine.control.event.Event._
import arx.gui2.Widget
import arx.gui2.WindowingSystem2
import arx.resource.ResourceManager

class ScrollBar(forWidget:Widget) extends Widget(forWidget) with TNonDiscoverable {
	var dockingSide : Moddable[Int] = Right

	this.x = () => dockingSide.resolve() match {
		case Right => forWidget.width - (forWidget.internalPaddingX * 2.0f) - this.width
		case Left => 0.0f
	}
	this.y = 0.0f
	width = 3.0f
	height = () => forWidget.clientHeight

	subPositioningIndependent = true
	backgroundImage = ResourceManager.image(WindowingSystem2.conf.defaultScrollBarBackground.str)
	pixelScale = 1

	val maxScroll = new CachedFloat(math.max(-forWidget.minChildY,0.0f))
	val minScroll = new CachedFloat(-forWidget.maxChildY + forWidget.clientHeight)
	def scrollRange = maxScroll - minScroll

	def setScrollByYPosition ( posy : Float ) {
		val relativeY = posy - this.absolutePosition.y - clientY
		val start = upButton.y + upButton.height + scrollBubble.height * 0.5f
		val end = downButton.y - scrollBubble.height * 0.5f
		val pcnt = (1.0f - (relativeY - start) / (end - start))
		forWidget.scrollY = (minScroll.resolve() + scrollRange * pcnt).clamp(minScroll,maxScroll)
	}
	def moveScrollPositionBy ( delta : Float ) {
		forWidget.scrollY = (forWidget.scrollY + delta).clamp( minScroll, maxScroll )
	}

	var scrollSpeed = 1.0f
	protected val scrollBase = 0.35f

	val upButton = new ImageButton(this)
	upButton.image = ResourceManager.getImage("ui/scrollArrowUp.png")
	upButton.maintainAspectRatio = true
	upButton.maintainExactPixelScale = true
	upButton.anchorTo = Top
	upButton.width = () => this.clientWidth
	upButton.height = () => this.clientWidth * 1.1f
	upButton.centerHorizontally()

	val downButton = new ImageButton(this)
	downButton.image = ResourceManager.getImage("ui/scrollArrow.png")
	downButton.maintainAspectRatio = true
	downButton.maintainExactPixelScale = true
	downButton.anchorTo = Bottom
	downButton.width = () => this.clientWidth
	downButton.height = () => this.clientWidth * 1.1f
	downButton.centerHorizontally()
	downButton.dockToInteriorBottom()

	val scrollBubble = new ImageDisplayWidget(this)
	scrollBubble.image = ResourceManager.getImage("ui/scrollBubble.png")
	scrollBubble.width = () => this.clientWidth.resolve()
	scrollBubble.height = () => this.clientWidth.resolve() * 2.0f
	scrollBubble.y = () => {
		val scrollPcnt = (1.0f - (forWidget.scrollY - minScroll) / scrollRange)
		val start = upButton.y + upButton.height
		val end = downButton.y - scrollBubble.height
		start + (end - start) * scrollPcnt
	}

	scrollBubble.consumeEvent {
		case MouseDragEvent(pos,_,_,_) => {
			setScrollByYPosition(pos.y)
		}
	}

	var maximumWheelJerk = scrollBase * 1.25f
	var desiredWheelDelta = 0.0f
	var currentWheelDelta = 0.0f

	this.onEvent {
		case MousePressEvent(_,pos,_) => setScrollByYPosition(pos.y)
		case MouseDragEvent(pos,_,_,_) => setScrollByYPosition(pos.y)
		case ScrollEvent(delta,modifiers) =>
			desiredWheelDelta = delta.y * scrollBase * 0.0075f
		//moveScrollPositionBy(delta.toFloat * scrollBase * 0.01f)
	}



	forWidget.onEvent {
		case ScrollEvent(delta,modifiers) =>
			desiredWheelDelta = delta.y * scrollBase * 0.0075f
		//moveScrollPositionBy(delta.toFloat * scrollBase * 0.01f)
	}

	override def updateLogic(f: Float) {
		var delta = 0.0f
		if ( upButton.isPressed ) { delta = scrollSpeed * scrollBase }
		else if ( downButton.isPressed ) { delta = -scrollSpeed * scrollBase }

		if ( absf(delta) > 0.00001f ) {
			moveScrollPositionBy(delta)
		}

		if ( desiredWheelDelta > currentWheelDelta ) {
			currentWheelDelta = (currentWheelDelta + maximumWheelJerk).min(desiredWheelDelta)
		} else if ( desiredWheelDelta < currentWheelDelta ) {
			currentWheelDelta = (currentWheelDelta - maximumWheelJerk).max(desiredWheelDelta)
		}
		if ( absf(currentWheelDelta) > 0.0001f ) {
			moveScrollPositionBy(currentWheelDelta)
		}
		desiredWheelDelta = 0.0f
	}

	override def close() {
		super.close()
	}

}
