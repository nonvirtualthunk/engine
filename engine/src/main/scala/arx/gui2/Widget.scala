package arx.gui2

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/30/13
 * Time: 12:19 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.application.Application
import arx.application.Noto
import arx.core.datastructures.MultiMap
import arx.core.parsing.ParseUtil
import arx.core.parsing.ParseUtil.EmptyExpression
import arx.core.parsing.ParseUtil.SimpleExpression
import arx.core.representation.ConfigValue
import arx.core.representation.Hocon
import arx.core.traits.TSentinel
import arx.core.traits.TSentinelable
import arx.core.vec.ReadVec3f
import arx.core.vec.ReadVec4f
import arx.core.vec.Vec3f
import arx.core.vec.Vec4f
import arx.core.ForwardingModdable
import arx.core.Moddable
import arx.core.math.Rectf
import arx.engine.control.event.Event.TEventUser
import arx.graphics.AVBO
import arx.graphics.Image
import arx.gui2.rendering.WidgetBackgroundComponent
import arx.gui2.rendering.WidgetRenderingComponent
import arx.gui2.widgets.ScrollBar
import arx.resource.ResourceManager

import scala.annotation.tailrec
import scala.collection.mutable
import arx.core.ImplicitModdable._
import arx.core.async.Executor
import arx.core.introspection.ReflectionAssistant
import arx.engine.control.event.Event.MouseReleaseEvent

class Widget ( parentis : Option[Widget] ) extends TEventUser with TSentinelable {
	def this ( w : Widget ) { this(if ( w != null ) { Some(w) } else { None }) }
	//+====================+ Hierarchy +====================+
	var id : String = ""
	var parent : Option[Widget] = parentis
	def parentOE = parent.getOrElse(Widget.Sentinel)
	var children : List[Widget] = Nil
	// TODO: how do we actually want to track this?
	protected[gui2] var _windowingSystem = none[WindowingSystem2]
	@tailrec final def windowingSystem : WindowingSystem2 = if ( _windowingSystem.isEmpty && parent.nonEmpty ) { parent.get.windowingSystem } else { _windowingSystem.get }
	def windowingSystem_= (w : WindowingSystem2): Unit = {
		_windowingSystem = Some(w)
	}
	def contains(w:Widget) : Boolean = w == this || children.exists( c => c.contains(w) )


	parent.ifPresent(p => {
		p.children ::= this
		addNextEventUser(p)
	})
	

	final def initialize (): Unit = { onInitialize() }
	def onInitialize () {}

	var dataString : String = ""
	var data : Option[Any] = None
	def recursiveData : Option[Any] = if (data.nonEmpty) { data } else { parent match { case Some(p) => p.recursiveData ; case None => None } }
	var replacements : Map[String,Any] = Map()
	def replacement( str : String ) : Any = replacements.getOrElse(str, parent match { case Some(p) => p.replacement(str) ; case _ => None })
	var acceptsFocus : Moddable[Boolean] = true

	//+====================+ Drag and Drop +====================+
	var draggable = Moddable(false)
	var droppable = Moddable(false)
	var dragData : Moddable[Any] = Moddable(dataString _)

	//+====================+ Appearance +====================+
	var backgroundImage : Moddable[Image] = Widget.DefaultImage
	var _backgroundColor = Moddable(Vec4f.One)
	def backgroundColor = _backgroundColor
	def backgroundColor_= ( b : Moddable[ReadVec4f] ) { _backgroundColor = b }
	var _edgeColor = Moddable(Vec4f.One)
	def edgeColor = _edgeColor
	def edgeColor_= ( b : Moddable[ReadVec4f] ) { _edgeColor = b }
	var segmentedBackground = Moddable(true)
	var drawCenterBackground = Moddable(true)
	var pixelScale = 1

	//+====================+ Extents and Positioning +====================+
	var x = Moddable(0.0f)
	var y = Moddable(0.0f)
	var z = Moddable(0.0f)

	def centerX_= ( cx : Moddable[Float] ) { x = Moddable(() => cx - width * 0.5f) }
	def centerY_= ( cy : Moddable[Float] ) { y = Moddable(() => cy - height * 0.5f) }
	def centerX = x + width * 0.5f
	def centerY = y + height * 0.5f

	var desiredWidth = Moddable(3.0f)
	var desiredHeight = Moddable(3.0f)
	def width = math.min(desiredWidth.resolve(),maxWidth.resolve())
	def height = math.min(desiredHeight.resolve(),maxHeight.resolve())
	def width_= ( w : Moddable[Float] ) { desiredWidth = w }
	def height_= ( h : Moddable[Float] ) { desiredHeight = h }

	var minWidth : Moddable[Float] = Moddable(5.0f)
	var minHeight : Moddable[Float] = Moddable(5.0f)

	var maxWidth  : Moddable[Float] = new ForwardingModdable[Float](desiredWidth _)
	var maxHeight : Moddable[Float] = new ForwardingModdable[Float](desiredHeight _)

	var internalPaddingX : Moddable[Float] = Moddable(0.0f)//() => if (segmentedBackground && backgroundImage.notSentinel){(WidgetBackgroundComponent.imageMetrics( backgroundImage, false ).borderPixelWidth.toFloat / EngineCore.windowWidth) * WindowingSystem.dimensions.x * pixelScale - 0.0001f} else {0.0f}
	var internalPaddingY : Moddable[Float] = Moddable(0.0f)//() => if (segmentedBackground && backgroundImage.notSentinel){(WidgetBackgroundComponent.imageMetrics( backgroundImage, false ).borderPixelWidth.toFloat / EngineCore.windowHeight) * WindowingSystem.dimensions.y * pixelScale} else {0.0f}

	var externalPaddingX = Moddable(0.0f)
	var externalPaddingY = Moddable(0.0f)

	var borderWidth = Moddable(0.0f)
	var borderHeight = Moddable(0.0f)

	var interstitialNearX = Moddable(0.0f)
	var interstitialNearY = Moddable(0.0f)
	var interstitialFarX = Moddable(0.0f)
	var interstitialFarY = Moddable(0.0f)

	var subPositioningIndependent = false

	def clientX = internalPaddingX + interstitialNearX
	def clientY = internalPaddingY + interstitialNearY
	def clientWidth = width - interstitialFarX - interstitialNearX - internalPaddingX * 2.0f
	def clientHeight = height - interstitialFarY - interstitialNearY - internalPaddingY * 2.0f
	def maxClientWidth = maxWidth - interstitialFarX - interstitialNearX - internalPaddingX * 2.0f
	def maxClientHeight = maxHeight - interstitialFarY - interstitialNearY - internalPaddingY * 2.0f

	def position = Vec3f(x,y,z)
	def absolutePosition : ReadVec3f = this.parent match {
		case Some(p) =>
			if (subPositioningIndependent) {
				p.absolutePosition + Vec3f(p.clientX + x,p.clientY + y,0.0f)
			} else {
				p.absolutePosition + Vec3f(p.clientX + p.scrollX + x,p.clientY + p.scrollY + y,0.0f)
			}
		case _ => position
	}

	def containsPoint ( point: Vec3f ): Boolean = {
		val epos = this.absolutePosition
		point.x > epos.x && point.x < epos.x + this.width &&
			point.y > epos.y && point.y < epos.y + this.height
	}

	//+====================+ Scaling +====================+
	var scaleX = Moddable(1.0f)
	var scaleY = Moddable(1.0f)

	//+====================+ Sub Positioning +====================+
	var scrollX = Moddable(0.0f)
	var scrollY = Moddable(0.0f)

	var scrollBar : Option[ScrollBar] = None
	def enableScrollBar (): Unit = {
		if (scrollBar.nonEmpty) { scrollBar.get.close() }
		val sb = new ScrollBar(this)
		scrollBar = Some(sb)
		this.interstitialFarX = Moddable(sb.width)
	}

	def autoScrollBar (): Unit = {
		if (scrollBar.nonEmpty) { scrollBar.get.close() }
		val sb = new ScrollBar(this)
		scrollBar = Some(sb)
		sb.showingCondition = Moddable(needsScrollBar _)
		this.interstitialFarX = Moddable(() => if (sb.showing) { sb.width } else { 0.0f })
	}
	protected def needsScrollBar = {
		children.exists(c => c.y + c.height > this.height)
	}

	//+====================+ Rendering +====================+

	var renderers = List[WidgetRenderingComponent](WidgetBackgroundComponent)

	protected var _showing = true
	var showingCondition = Moddable[Boolean](true)
	def showing = _showing && showingCondition

	var closed = false

	def hide () { _showing = false }
	def show () { _showing = true }

	def close () {
		closed = true
		parentis match {
			case Some(p) => p.children = p.children without this
			case None => windowingSystem.removeTopLevelWidget(this)
		}
		windowingSystem.modalWidgetStack = windowingSystem.modalWidgetStack.filterNot(_._1 == this)
	}

	def makeEphemeral () {
		windowingSystem.modalWidgetStack ::= (this -> true)
	}
	def makeModal (): Unit = {
		windowingSystem.modalWidgetStack ::= (this -> false)
	}

	//+====================+ Update +====================+
	protected[gui2] def updateSubsystem ( f : Float ) {
		updateLogic(f)
		for ( child <- children ){
			child.updateSubsystem(f)
		}
	}

	protected def updateLogic ( f : Float ){}
	protected def updateUI (){}

	//+====================+ Drawing +====================+
	var lastDrawn = -1
	var onUpdateUI : List[() => Unit] = Nil

	/**
	 * Precondition: matrix state is aligned to top left
	 */
	def draw (vbo: AVBO, context: WindowingSystem2.RenderingContext ) {
		updateUI()
		onUpdateUI.foreach(f => f())
		if ( showing ) {
			context.translationBlock(x,y,z,scaleX,scaleY,1.0f) {
				lastDrawn = WindowingSystem2.drawingTicksDone
				drawSelf(vbo,context)

				if ( children.nonEmpty ) {
					var hasSubPosIndependentChildren = false
					context.translationBlock(clientX + scrollX,clientY + scrollY,0.0f,1.0f,1.0f,1.0f) {
						boundedBlock(context){
							children = children.sortBy(_.z.resolve())
							for ( c <- children ) {
								if (! c.subPositioningIndependent) { c.draw(vbo,context) }
								else { hasSubPosIndependentChildren = true }
							}
						}
					}

					if (hasSubPosIndependentChildren) {
						context.translationBlock(internalPaddingX,internalPaddingY,0.0f,1.0f,1.0f,1.0f) {
							subBoundedBlock(context,true,true) {
								for (c <- children if c.subPositioningIndependent) {
									c.draw(vbo,context)
								}
							}
						}
					}
				}

				drawSelfAfter(vbo,context)
			}
		}
	}

	protected def drawSelf (vbo:AVBO,context:WindowingSystem2.RenderingContext ) {
		renderers.foreach( rc => rc.draw(this,vbo,context,beforeChildren = true) )
	}
	protected def drawSelfAfter(vbo:AVBO,context:WindowingSystem2.RenderingContext ) {
		var index = 0
		renderers.foreach( rc => {
			context.translationBlock(0.0f,0.0f,0.01f * index,1.0f,1.0f,1.0f) {
				rc.draw(this,vbo,context,beforeChildren = false)
			}
			index += 1
		} )
	}

	def boundedBlock (context: WindowingSystem2.RenderingContext)(stmt: => Unit) {
		subBoundedBlock(context,false,false)(stmt)
	}
	def selfBoundedBlock (context: WindowingSystem2.RenderingContext)(stmt: => Unit) {
		subBoundedBlock(context,true,false)(stmt)
	}
	protected def subBoundedBlock (context: WindowingSystem2.RenderingContext, self : Boolean, subPosIndependent : Boolean  )(stmt: => Unit) {
		val epos: Vec3f = context.translation
		val parentBounds = Rectf(context.boundsStack.head)
		val w = if (self && subPosIndependent) { width - internalPaddingX * 2.0f } else if (self) { width } else { clientWidth }
		val h = if (self && subPosIndependent) { height - internalPaddingY * 2.0f } else if (self) { height } else { clientHeight }
		val personalBounds = Rectf(
			roundf(context.toPixelScaleX(epos.x - (if (!subPosIndependent){scrollX:Float} else {0.0f}) - 0.5f)),
			roundf(context.toPixelScaleY(epos.y - (if (!subPosIndependent){scrollY:Float} else {0.0f}) - 0.5f)),
			roundf(context.toPixelScaleX(w + 0.5f)),
			roundf(context.toPixelScaleY(h + 0.5f))
		)
		val bounds = parentBounds.intersect(personalBounds)
		context.boundsStack = bounds :: context.boundsStack

		stmt

		context.boundsStack = context.boundsStack.drop(1)
	}

	def hasFocus = windowingSystem.focusedWidget == Some(this)

	//+====================+ SML Interface +====================+
	
	def setFromSML(sml: ConfigValue,overwrite : Boolean) {
		id = sml.id.strOrElse(id)

		if (sml.style.nonEmptyValue) {
			setFromSML(sml.style,overwrite = false)
		}


		var childrenToSML = Map[Widget,ConfigValue]()
		if ( sml.children.nonEmptyValue ) {
			val smlWithIds = if (sml.children.isArr) { sml.children.arr.map(s => s.id.strOrElse("------") -> s) }
			else { sml.children.fields.toList }
			for ( (cid,child) <- smlWithIds) {
				val childWidget = children.find ( _.id == cid ) match {
					case Some(w) => w
					case None => {
						val widgetType = child.widget.strOrElse("widget")
						Widget.createWidgetWithTypeString(widgetType,this)
					}
				}
				childWidget.id = cid
				childrenToSML += childWidget -> child
			}
		}

		for ((child,sml) <- childrenToSML) {
			child.setFromSML(sml,overwrite)
		}

		val xSml = if (sml.hasField("x")) { sml.field("x") } else if (sml.hasField("pos")) { sml.pos.arr(0) } else { ConfigValue.Sentinel }
		val ySml = if (sml.hasField("y")) { sml.field("y") } else if (sml.hasField("pos")) { sml.pos.arr(1) } else { ConfigValue.Sentinel }
		if ( xSml.nonEmptyValue ) {
			xSml.str match {
				case "centered" => x = Moddable(() => (parentOE.clientWidth - this.width) * 0.5f)
				case "right-aligned" => x = Moddable(() => parentOE.clientWidth - this.width)
				case s if s.endsWith("px") => x = Moddable(windowingSystem.widgetUnitsPerPixelX * s.dropRight(2).toFloat)
				case o => {
					ParseUtil.parsePrefixAndParens(o,recurse = false) match {
						case EmptyExpression => x = Moddable(xSml.float)
						case SimpleExpression(kind,arg) =>
							lazy val sib = parentOE.getById[Widget](arg)
							kind match {
								case "right-of" => x = Moddable(() => sib.x + sib.width)
								case "left-of" => x = Moddable(() => sib.x - this.width)
								case "right-aligned" => x = Moddable(() => parentOE.clientWidth - this.width - arg.toFloat)
								case "same-as" => x = new ForwardingModdable[Float](sib.x _)
								case _ => Noto.warn(s"Unknown position expression : $o")
							}
					}
				}
			}
		}
		if ( ySml.nonEmpty ) {
			ySml.str match {
				case "centered" => y = Moddable(() => (parentOE.clientHeight - this.height) * 0.5f)
				case "bottom-aligned" => y = Moddable(() => parentOE.clientHeight - this.height)
				case s if s.endsWith("px") => y = Moddable(windowingSystem.widgetUnitsPerPixelY * s.dropRight(2).toFloat)
				case o => {
					ParseUtil.parsePrefixAndParens(o,recurse = false) match {
						case EmptyExpression => y = Moddable(ySml.float)
						case SimpleExpression(kind,arg) =>
							lazy val sib = parentOE.getById[Widget](arg)
							kind match {
								case "below" => y = Moddable(() => sib.y + sib.height)
								case "above" => y = Moddable(() => sib.y - this.height)
								case "bottom-aligned" => y = Moddable(() => parentOE.clientHeight - this.height - arg.toFloat)
								case "same-as" => y = new ForwardingModdable[Float](sib.y _)
								case _ => Noto.warn(s"Unknown position expression : $o")
							}
					}
				}
			}
		}

		z = sml.z.floatOrElse(z)


		dimensionFromSML(sml.width,w = true)
		dimensionFromSML(sml.height,w = false)
		pixelScale = sml.pixelScale.intOrElse(pixelScale)

		if ( sml.background.nonEmptyValue ) {
			val imgStr = sml.background.str
			if ( imgStr == "none" ) {
				backgroundImage = Moddable(Image.Sentinel)
			} else {
				backgroundImage = Moddable(ResourceManager.image( imgStr ))
			}
		}
		if ( sml.backgroundColor.nonEmptyValue ) { backgroundColor = sml.backgroundColor.v4 }
		if ( sml.edgeColor.nonEmptyValue ) { edgeColor = sml.edgeColor.v4 }
		dataString = sml.data.strOrElse(dataString)

		acceptsFocus = sml.acceptsFocus.boolOrElse(acceptsFocus)
		segmentedBackground = sml.segmentedBackground.boolOrElse(segmentedBackground)

		if (sml.showing.nonEmptyValue) {
			showingCondition = Moddable(sml.showing.bool)
		}
		if (sml.scrollBar.boolOrElse(false)) {
			enableScrollBar()
		}
		if (sml.modal.boolOrElse(false)) {
			makeModal()
		} else if (sml.ephemeral.boolOrElse(false)) {
			makeEphemeral()
		}
	}

	protected def dimensionFromSML ( sml : ConfigValue, w : Boolean ) = {
		if ( sml.nonEmpty ) {
			val unwrapped = sml.unwrapped
			val convUnwrapped = unwrapped match {
				case i : Int => i.toFloat
				case o => o
			}

			convUnwrapped match {
				case str : String => {
					if ( str.endsWith("%") ) {
						try {
							parent match {
								case Some(p) =>
									if ( w ) { width = () => p.clientWidth * (str.dropRight(1).toFloat * 0.01f) }
									else { height = () => p.clientHeight * (str.dropRight(1).toFloat * 0.01f) }
								case None => Noto.warn("Percentage width being applied to parentless widget")
							}
						} catch {
							case e : Exception => Noto.warn("Percentage for width was malfored :" + str)
						}
					} else if (str.contains("(")) {
						ParseUtil.parsePrefixAndParens(str,recurse = false) match {
							case EmptyExpression => Noto.warn("Invalid dimension expression")
							case SimpleExpression(kind,arg) => {
								lazy val sib = parentOE.getById[Widget](arg)
								kind match {
									case "stretch-to" =>
										if (w) {
											width = () => sib.x - this.x
										} else {
											height = () => sib.y - this.y
										}
									case "match" if w => width = () => sib.width
									case "match" if !w => height = () => sib.height
									case _ => Noto.warn("Invalid dimension expression : " + kind)
								}
							}
						}
					} else {
						Noto.warn("Width string could not be interpreted : " + str)
					}
				}
				case f : Float => {
					if ( f < 0.0f ) {
						parent match {
							case Some(p) =>
								if ( w ) { width = () => p.clientWidth + f }
								else { height = () => p.clientHeight + f }
							case None => Noto.warn("Negative, relative width being applied to parentless widget")
						}
					} else {
						if ( w ) { width = f }
						else { height = f }
					}
				}
				case _ => Noto.warn("Unknown sml value type for setting width")
			}
		}
	}

	protected def SMLTypeIdentifier = "widget"

	def getById[T <: Widget : Manifest] ( idToGet: String ) : T = {
		if ( idToGet == id && manifest[T].runtimeClass.isAssignableFrom(this.getClass) ) { this.asInstanceOf[T] }
		else {
			for ( child <- children ) {
				child.getByIdOpt(idToGet) match {
					case Some(w) => {
						if ( manifest[T].runtimeClass.isAssignableFrom(w.getClass) ) { return w.asInstanceOf[T] }
					}
					case None => //do nothing
				}
			}
			Noto.warn(s"getById[${manifest[T].runtimeClass.getSimpleName}]($idToGet) could not find appropriate child of widget ($id), returning new stand-in")
			Widget.instantiateWidgetClass(manifest[T].runtimeClass,this).asInstanceOf[T]
		}
	}
	
	def child(idToGet:String) = getById[Widget](idToGet)

	def getByIdOpt[T <: Widget : Manifest] ( idToGet: String ) : Option[T] = {
		if ( idToGet == id && manifest[T].runtimeClass.isAssignableFrom(this.getClass) ) { Some(this.asInstanceOf[T]) }
		else {
			for ( child <- children ) {
				child.getByIdOpt(idToGet) match {
					case Some(w) => {
						if ( manifest[T].runtimeClass.isAssignableFrom(w.getClass) ) { return Some(w.asInstanceOf[T]) }
					}
					case None => //do nothing
				}
			}
			None
		}
	}
}

object Widget {
	val Sentinel : Widget = new Widget(None) with TSentinel

	val DefaultImage = Moddable(ResourceManager.image("ui/singlePixelBorderWhite_ne.png"))

	protected val resourceBackedWidgets = new MultiMap[(String,String),Widget]
	protected val lastModifiedMarkers = new mutable.HashMap[(String,String),Long]
	if ( ResourceManager.useLocalResources ) {
		Executor.schedulePeriodic(5.seconds,() => {
			var countdown = 10.0f
			def update(f: Float): Unit = {
				countdown -= f
				if ( countdown < 0.0f ) {
					countdown = 10.0f

					for ( ((k,sub),values) <- resourceBackedWidgets.intern ) {
						val file = ResourceManager.file(k)
						val marker = lastModifiedMarkers.getOrElseUpdate((k,sub),file.lastModified)
						val current = file.lastModified
						if ( current > marker ){
							Noto.info(s"Reloading sml-backed widgets originating from $k")
							lastModifiedMarkers((k,sub)) = current
							try {
								val newSML = Hocon.parseFile(file)
								ResourceManager.state.sml.put(k,newSML)
								for ( w <- values ) {
									w.setFromSML( (if(sub.isEmpty){newSML}else{newSML.field(sub)}) , overwrite = false )
								}
							} catch {
								case e : Exception => Noto.error(f"Failed to process updated widget sml, exception was $e")
							}
						}
					}
				}
			}
		})
	}

	lazy val allWidgetClasses = classOf[Widget] :: ReflectionAssistant.allSubTypesOf(classOf[Widget])
	lazy val allWidgetInstances = allWidgetClasses.map( clazz => instantiateWidgetClass(clazz,null) ).filterNot( _ == null )
	protected def instantiateWidgetClass( clazz : Class[_], parent : Widget ) = {
		try {
			clazz.getConstructor(classOf[Widget]).newInstance(parent).asInstanceOf[Widget]
		} catch {
			case e : Exception => null
		}
	}

	def createWidgetWithTypeString ( typeString : String , parent : Widget ) = {
		allWidgetInstances.find( inst => inst.SMLTypeIdentifier.toLowerCase == typeString.toLowerCase ) match {
			case Some(w) => instantiateWidgetClass( w.getClass , parent )
			case None =>
				Noto.warn(f"No widget class found for typestring $typeString")
				Widget.Sentinel
		}
	}

	def fromResource ( resourcePath : String , subPath : String, parent : Widget ) = {
		val w = fromSML( ResourceManager.sml(resourcePath).field(subPath), parent )
		if (subPath != "") {
			if (w.id == "") { w.id = subPath }
		}
		resourceBackedWidgets.add(resourcePath -> subPath,w)
		w
	}
	def fromResource ( resourcePath : String , parent : Widget ) = {
		val w = fromSML( ResourceManager.sml(resourcePath), parent )
		resourceBackedWidgets.add(resourcePath -> "",w)
		w
	}
	def fromResourceTyped[T : Manifest] ( resourcePath : String , parent : Widget ) : T= {
		val w = fromSML( ResourceManager.sml(resourcePath), parent )
		resourceBackedWidgets.add(resourcePath -> "",w)
		w.asInstanceOf[T]
	}
	def fromResourceTyped[T : Manifest] ( resourcePath : String , subPath : String, parent : Widget ) : T= {
		val w = fromSML( ResourceManager.sml(resourcePath).field(subPath), parent )
		resourceBackedWidgets.add(resourcePath -> subPath,w)
		w.asInstanceOf[T]
	}

	def createFromSML (sml : ConfigValue, parent : Widget) = fromSML(sml,parent)

	private def fromSML ( sml : ConfigValue , parent : Widget ) = {
		val widgetType = sml.widget.strOrElse("widget")
		val widget = createWidgetWithTypeString(widgetType,parent)
		widget.setFromSML(sml,true)

		widget
	}

	implicit class RichWidget ( val widget : Widget ) {
		def onClick[T] ( f : => T ) { widget.onEvent {
			case MouseReleaseEvent(_,_,_) => f
		}}

		def consumeClick[T] ( f : => T ) { widget.consumeEvent {
			case MouseReleaseEvent(_,_,_) => f
		}}

		def takeFocus () {
			widget.windowingSystem.giveFocusTo(widget)
		}

		def centerHorizontally () {
			widget.x = () => (widget.parentOE.clientWidth - widget.width) * 0.5f
		}
		def centerVertically () {
			widget.y = () => (widget.parentOE.clientHeight - widget.height) * 0.5f
		}
		def dockToInteriorBottom (buffer:Float = 0.0f) {
			widget.y = () => (widget.parentOE.clientHeight + 0.05f - widget.height - buffer)
		}
		def dockToInteriorRight (buffer : Float = 0.0f) {
			widget.x = () => (widget.parentOE.clientWidth - widget.width - buffer)
		}
		def dockRight ( of : Widget, buffer : Float = 0.0f ) {
			widget.x = () => of.x + of.width + buffer
		}
		def dockBelow ( other : Widget ): Unit = {
//			widget.y = () => {
//				val y = other.windowingSystem.widgetCoordinatesToPixelCoordinatesY(other.y)
//				val h = other.windowingSystem.widgetHeightToPixelHeight(other.height,round = true)
//				widget.windowingSystem.pixelYtoWidgetY(y + h)
//			}
			widget.y = () => other.y + other.height
		}

		def minChildY = {
			widget.children.fmin(_.y)
		}
		def minChildX = {
			widget.children.fmin(_.x)
		}
		def maxChildY = {
			widget.children.fmax(c => c.y + c.height)
		}
		def maxChildX = {
			widget.children.fmax(c => c.x + c.width)
		}
	}
}
