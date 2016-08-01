package arx.gui2

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/30/13
 * Time: 3:44 PM
 * To change this template use File | Settings | File Templates.
 */

import java.awt.Toolkit
import java.awt.datatransfer._
import java.io.IOException
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.LockSupport

import arx.Prelude._
import arx.application.Application
import arx.application.Noto
import arx.core.datastructures.Killable
import arx.core.datastructures.KillableThread
import arx.core.mat.Mat4x4
import arx.core.math.Rectf
import arx.core.metrics.Metrics
import arx.core.traits.TSentinel
import arx.core.traits.TSentinelable
import arx.core.vec.ReadVec2f
import arx.core.vec.ReadVec3f
import arx.core.vec.Vec2f
import arx.core.vec.Vec3f
import arx.engine.EngineCore
import arx.engine.control.event.Event._
import arx.graphics._
import arx.graphics.shader.Shader
import arx.graphics.text._
import arx.gui2.WindowingSystem2.RenderingContext
import arx.gui2.events.DropEvent
import arx.gui2.events.FocusGainedEvent
import arx.gui2.events.FocusLostEvent
import arx.gui2.rendering.WindowingSystemAttributeProfile2
import arx.gui2.widgets.Dialog
import arx.gui2.widgets.OpenGLWidget
import arx.resource.ResourceManager
import com.codahale.metrics.ConsoleReporter
import org.lwjgl.glfw.GLFW
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._

import scala.collection.mutable

class WindowingSystem2(engine : EngineCore) extends TEventUser with TSentinelable {
	protected var _topLevelWidgets = List[Widget]()
	def topLevelWidgets = _topLevelWidgets
	def addTopLevelWidget ( w : Widget ) { _topLevelWidgets = (w :: _topLevelWidgets).sortBy( _.z.resolve() ); w._windowingSystem = Some(this); w.initialize(); }
	def removeTopLevelWidget ( w : Widget ) { _topLevelWidgets = _topLevelWidgets without w }
	var currentPressedWidget: Option[Widget] = None
	var lastWidgetUnderMouse: Option[Widget] = None
	var modalWidgetStack : List[(Widget,Boolean)] = Nil
	var dragAndDropWidget : Option[Widget] = None

	val vbo : AVBO = new AVBO(WindowingSystemAttributeProfile2)
	vbo.state.set(VBO.Dirty)

	val mainTextureBlock = new TextureBlock(4096,4096)
	mainTextureBlock.borderWidth = 0
//	mainTextureBlock.minFilter = WindowingSystem.getProperty("minFilter","nearest") match { case "nearest" => GL_NEAREST ; case "linear" => GL_LINEAR ; case "mipmap" => GL_LINEAR_MIPMAP_LINEAR }
//	mainTextureBlock.magFilter = WindowingSystem.getProperty("magFilter","nearest") match { case "nearest" => GL_NEAREST ; case "linear" => GL_LINEAR ; case "mipmap" => GL_LINEAR_MIPMAP_LINEAR }
	mainTextureBlock.minFilter = GL_LINEAR
	mainTextureBlock.magFilter = GL_NEAREST


	var loadedFonts = new mutable.HashMap[String,TBitmappedFont]()

	def loadFont (fontName : String) = {
		loadedFonts.getOrElseUpdate(fontName,ResourceManager.getFont(fontName,mainTextureBlock))
	}

	// TODO: make font creation ordering not break everything, stupid AWT
	lazy val font : TBitmappedFont = if ( ! isSentinel ) {
		loadFont(WindowingSystem2.conf.defaultFont.str)
	} else {
		null
	}



	var lastMousePosition : Vec3f = Vec3f(0.0f,0.0f,0.0f)
	var lastWidgetMousePosition = Vec3f(0.0f,0.0f,0.0f)

	var drawUI = true
	var shader = ResourceManager.shader("shaders/windowing/main")

	var _focusedWidgetStack : List[Widget] = List()

	//+====================+ Event Handling +====================+
	override def handleEvent (event: Event): Boolean = {
		event match {
			case MousePressEvent(button,_,modifiers) => getWidgetForMousePos(topLevelWidgets,pixelPos2widgetPos(lastMousePosition)) match {
				case Some(widg) => {
					var passOn = true
					if ( modalWidgetStack.nonEmpty ) {
						if ( ! modalWidgetStack.head._1.contains(widg) ) {
							passOn = false
							if ( modalWidgetStack.head._2 ) {
								modalWidgetStack.head._1.close()
							}
						}
					}
					if ( passOn ) {
						giveFocusTo(widg)
						currentPressedWidget = Some(widg)
						widg.handleEvent(MousePressEvent(button,pixelPos2widgetPos(lastMousePosition).xy,modifiers).withOrigin(widg))
					}
					passOn
				}
				case _ => {
					currentPressedWidget = None
					false
				}
			}
			case MouseReleaseEvent(button,pos,modifiers) if button != -1 => getWidgetForMousePos(topLevelWidgets,Vec3f(pixelPos2widgetPos(pos),0.0f)) match {
				case Some(widg) => {
					dragAndDropWidget match {
						case Some(dd) =>
							var tmp : Option[Widget] = Some(widg)
							while (tmp.nonEmpty && !tmp.get.droppable) {
								tmp = tmp.get.parent
							}

							if (tmp.nonEmpty) {
								val droppedOn = tmp.get
								val localPos = Vec2f(pixelPos2widgetPos(pos) - droppedOn.absolutePosition.xy)
								localPos.x += droppedOn.interstitialNearX + droppedOn.internalPaddingX
								localPos.y += droppedOn.interstitialNearY + droppedOn.internalPaddingY
								droppedOn.handleEvent(DropEvent(dd,droppedOn,dd.dragData.resolve(),localPos))
							}
							dragAndDropWidget = None
							true
						case None => {
							currentPressedWidget = None
							widg.handleEvent(MouseReleaseEvent(button,pixelPos2widgetPos(pos),modifiers).withOrigin(widg))
						}
					}
				}
				case _ => {
					dragAndDropWidget match {
						case Some(dd) =>
							dragAndDropWidget = None
							true
						case None => {
							currentPressedWidget = None
							false
						}
					}
				}
			}
			case MouseMoveEvent(pos,delta,modifiers) =>
				lastWidgetMousePosition = Vec3f(pixelPos2widgetPos(pos),0.0f)
				lastMousePosition = Vec3f(pos,0.0f)
				getWidgetForMousePos(topLevelWidgets,lastWidgetMousePosition) match {
					case Some(widg) => {
						lastWidgetUnderMouse = Some(widg)
						widg.handleEvent(MouseMoveEvent(pixelPos2widgetPos(pos),pixelDelta2WidgetDelta(Vec3f(delta,0.0f)).xy,modifiers).withOrigin(widg))
					}
					case _ =>
						lastWidgetUnderMouse = None
						false
				}
			case MouseDragEvent(pos,delta,button,modifiers) =>
				lastWidgetMousePosition = Vec3f(pixelPos2widgetPos(pos),0.0f)
				lastMousePosition = Vec3f(pos,0.0f)
				lastWidgetUnderMouse = getWidgetForMousePos(topLevelWidgets,pixelPos2widgetPos(Vec3f(pos,0.0f)))
				currentPressedWidget match {
					case Some(widg) => {
						var tmp : Option[Widget] = Some(widg)
						while (tmp.nonEmpty && ! tmp.get.draggable) {
							tmp = tmp.get.parent
						}
						if (tmp.nonEmpty) {
							dragAndDropWidget = tmp
							true
						} else {
							widg.handleEvent(MouseDragEvent(lastWidgetMousePosition.xy,pixelDelta2WidgetDelta(Vec3f(delta,0.0f)).xy,button,modifiers).withOrigin(widg))
						}
					}
					case _ =>
						false
				}
			case ScrollEvent(delta,modifiers) => lastWidgetUnderMouse match {
				case Some(widg) =>
					widg.handleEvent(ScrollEvent(delta,modifiers).withOrigin(widg))
					true
				case _ => false
			}
			case kpe: KeyPressEvent => {
				if ( kpe.key == GLFW.GLFW_KEY_F6) {
					drawUI = ! drawUI
				} else if ( kpe.key == GLFW.GLFW_KEY_F7 ) {
					Noto.info("+=Metrics===============+")
					val reporter = ConsoleReporter.forRegistry(Metrics.registry)
						.convertRatesTo(TimeUnit.SECONDS)
						.convertDurationsTo(TimeUnit.MILLISECONDS)
						.build()
					reporter.report(Metrics.registry.getGauges,Metrics.registry.getCounters,Metrics.registry.getHistograms,Metrics.registry.getMeters,Metrics.registry.getTimers)
//					reporter.report()
					Noto.info("\\=End===============/")

//					if ( ! GL.disabled ) { Analytics.clear() }
//					else {
//
//					}
//					GL.disabled = ! GL.disabled
				} else if ( kpe.key == GLFW.GLFW_KEY_F8 ) {
					if ( Noto.globalLoggingLevel != Noto.None ) { Noto.globalLoggingLevel = Noto.None }
					else { Noto.globalLoggingLevel = Noto.Info }
				} else if ( kpe.key == GLFW.GLFW_KEY_F9 ) {
					ResourceManager.refreshImages()
				}
				focusedWidget match {
					case Some(widg) =>
						widg.handleEvent(KeyPressEvent(kpe.key,kpe.modifiers).withAscii(kpe.asciiChar).withOrigin(widg))
					case _ =>
						false
				}
			}
			case kre: KeyReleaseEvent =>
				focusedWidget match {
					case Some(widg) =>
						widg.handleEvent(KeyReleaseEvent(kre.key,kre.modifiers).withAscii(kre.asciiChar).withOrigin(widg))
					case _ =>
						false
				}

			case _ => false
		}
	}
	
	//+====================+ Drawing +====================+ 
	var openglWidgetIndices = List[(Int,OpenGLWidget)]()
	
	val renderThread = new KillableThread(Killable.ApplicationLevel) {
		var lastRendered = 0L
		//		val interval = 10000000L
		val interval = 16000000L
		def whileRunningDo() {
			// TODO: re-enable the animvation vector clock...if we find what we were doing with that
//			AnimationVectorClock.enableForThisThread()

			val curTime = System.nanoTime
			val sinceLastRender = curTime - lastRendered
			if ( sinceLastRender > interval ) {
				lastRendered = curTime
				update()
			} else {
				LockSupport.parkNanos(interval / 2)
			}
		}

		def update () {

			if ( vbo.state.compareAndSet(VBO.Dirty,VBO.Updating) ) {
				val fullBounds = Rectf(0.0f,0.0f,EngineCore.pixelWidth,EngineCore.pixelHeight)
				val ratioX = EngineCore.pixelWidth.toFloat / dimensions.x
				val ratioY = EngineCore.pixelHeight.toFloat / dimensions.y
				val context = new RenderingContext(shader,mainTextureBlock,Vec2f(ratioX,ratioY))
				context.boundsStack ::= fullBounds

				WindowingSystem2.drawingTicksDone += 1

				vbo.numPoints = 0
				vbo.numIndices = 0
				for ( widget <- topLevelWidgets ) {
					try {
						widget.draw(vbo,context)
					} catch {
						case e : Exception =>
							Noto.warn("exception encountered during windowing system rendering, logging and continuing");
							Noto.warn("\tproblematic widget was : " + widget)
							e.printStackTrace()
					}
				}
				for ( dragW <- dragAndDropWidget ) {
					val p = lastWidgetMousePosition
					context.translationBlock(p.x - dragW.x - dragW.width * 0.5f,p.y - dragW.y - dragW.height * 0.5f,10.0f,1.0f,1.0f,1.0f) {
						dragW.draw(vbo,context)
					}
				}

				openglWidgetIndices = context.newOpenglWidgetIndices
				//						Noto.info("Opengl widgets : " + newOpenglWidgetIndices.size)
				context.newOpenglWidgetIndices = Nil
				vbo.state.set(VBO.Updated)
			}
		}
	}
	renderThread.start()
	
	def draw (){
		if ( drawUI ) {
			shader.bind()

			val projMatrix = GL.ortho(0.0f,(EngineCore.pixelWidth).toInt,(EngineCore.pixelHeight).toInt,0.0f,-100.0f,100.0f)
			shader.setUniform("ProjectionMatrix",projMatrix,tolerateAbsence = false)
			shader.setUniform("ModelViewMatrix",Mat4x4.Identity,tolerateAbsence = false)

//			if ( ! vbo.isSolidified ) {
				if ( vbo.solidifyIfNecessary(GL_STREAM_DRAW) ) { vbo.state.set(VBO.Dirty) }
//			}
			if ( vbo.isSolidified ) {
				restoreOpenGLContext()

				var icounter = 0
				for ( (startIndex,widget) <- openglWidgetIndices ) {
					GL.glDepthMask(false)
					vbo.drawElements(GL_TRIANGLES,icounter,startIndex - icounter)
					GL.glDepthMask(true)
					icounter = startIndex
					widget.subDrawGL()
					restoreOpenGLContext()
				}
				GL.glDepthMask(false)
				vbo.drawElements(GL_TRIANGLES,icounter)
				GL.glDepthMask(true)
			}


			Shader.unbind()
		} else {
			for ( (startIndex,widget) <- openglWidgetIndices ) {
				widget.subDrawGL()
			}
		}
	}

	/**
	 * Sets all of the settings back to normal for windowing system rendering (depth testing,
	 * texture bindings, shaders, etc), does nothing to matrices
	 */
	def restoreOpenGLContext () {
		shader.bind()
		shader.setUniform("mainTexture",0)

		GL.glSetState(GL_DEPTH_TEST,enable = false)
		GL.glSetState(GL_CULL_FACE,enable = false)

		mainTextureBlock.bind()
//		ResourceManager.getFont(WindowingSystem.defaultFont).bind(1)
//		font.bind(1)
	}


	//+====================+ Updating +====================+
	def update ( f : Float ) {
		Metrics.timer("Windowing System Update").timeStmt {
			updateFocusStack()

			topLevelWidgets.foreach( _.updateSubsystem(f) )
		}
	}

	//+====================+ Coordinate Systems +====================+
	def width = dimensions.x
	def height = dimensions.y
	def rawRatio = 1.0f
	def whRatio = EngineCore.pixelWidth.toFloat/EngineCore.pixelHeight.toFloat
	def dimensions = Vec3f(whRatio * 100.0f * rawRatio, 100.0f * rawRatio, 100.0f * rawRatio)
	def toPixelCoordinatesWithoutInvertedY ( v: Vec3f ) : Vec3f = {
		(v / dimensions) * Vec3f(EngineCore.pixelWidth.toFloat,EngineCore.pixelHeight.toFloat,1.0f)
	}
	def toPixelCoordinates ( v: ReadVec3f ) : ReadVec3f = {
		val normalizedPos : Vec3f = v / dimensions
		normalizedPos.y = 1.0f - normalizedPos.y
		normalizedPos * Vec3f(EngineCore.pixelWidth.toFloat,EngineCore.pixelHeight.toFloat,1.0f)
	}
	def pixelDelta2WidgetDelta ( delta : ReadVec3f ) : ReadVec3f = {
		val res = Vec3f(
			(delta.x / EngineCore.pixelWidth.toFloat) * dimensions.x,
			-(delta.y / EngineCore.pixelHeight.toFloat) * dimensions.y,
			delta.z
		)
		res
	}

	def pixelPos2widgetPos ( pos: ReadVec3f ) : ReadVec3f = {
		val res = pixelPos2widgetPos(pos.xy)
		Vec3f(res.x,res.y,pos.z)
	}
	def pixelPos2widgetPos ( pos: ReadVec2f ) : ReadVec2f = {
		Vec2f(
			(pos.x / EngineCore.pixelWidth.toFloat) * dimensions.x,
			((EngineCore.pixelHeight - pos.y - 1) / EngineCore.pixelHeight.toFloat)  * dimensions.y
		)
	}

	def pixelYtoWidgetY (y : Float) = {
		((EngineCore.pixelHeight - y - 1) / EngineCore.pixelHeight.toFloat)  * dimensions.y
	}
	def widgetUnitsPerPixelX = (dimensions.x / EngineCore.pixelWidth.toFloat)
	def widgetUnitsPerPixelY = (dimensions.y / EngineCore.pixelHeight.toFloat)

	def widgetCoordinatesToPixelCoordinatesX ( x : Float ) : Float = {
		floorf((x / dimensions.x) * EngineCore.pixelWidth + 0.0001f) + 0.5f
	}
	def widgetCoordinatesToPixelCoordinatesY ( y : Float ) : Float = {
		floorf((y / dimensions.y) * EngineCore.pixelHeight + 0.0001f) + 0.5f
	}
	def widgetWidthToPixelWidth ( w : Float , round : Boolean ) : Float = {
		if (round) { roundf((w / dimensions.x) * EngineCore.pixelWidth + 0.0001f) }
		else { floorf((w / dimensions.x) * EngineCore.pixelWidth + 0.0001f) }
	}
	def widgetHeightToPixelHeight ( h : Float , round : Boolean ) : Float = {
		if (round) { roundf((h / dimensions.y) * EngineCore.pixelHeight + 0.0001f) }
		else { floorf((h / dimensions.y) * EngineCore.pixelHeight + 0.0001f) }
	}
	def roundToExactPixel ( w : Float ) : Float = {
		val pixels = (w / dimensions.x) * EngineCore.pixelWidth
		(w / pixels) * pixels.round
	}

	//+====================+ Widget Picking +====================+
	def getWidgetForMousePos ( widgetList : List[Widget] , _pos: Vec3f): Option[Widget] = {
		val pos = if ( engine.mouseGrabbed ) { Vec3f(dimensions.x / 2.0f,dimensions.y / 2.0f,0.0f) } else { _pos }
		//Fold right so that the last drawn are the first picked
		widgetList.foldRight(None:Option[Widget])( { (widg,accum) =>
			accum match {
				case None =>
					if ( widg.showing && widg.containsPoint(pos) ) {
						getWidgetForMousePos(widg.children,pos).orElse(Some(widg))
					} else { None }
				case s: Some[Widget] => s
			}
		} )
	}

	//+====================+ Focus +====================+
	def focusedWidgetStack = {
		_focusedWidgetStack
	}
	def giveFocusTo ( widget : Widget ) {
		var effWidget = widget
		while (!effWidget.acceptsFocus && effWidget != null){
			effWidget = effWidget.parent.orNull
		}
		if (effWidget != null) {
			_focusedWidgetStack = _focusedWidgetStack match {
				case Nil => List(widget)
				case head :: tail => widget :: tail
			}
		}
	}
	def pushOntoFocusStack ( widget : Widget ) { addToFocusStack(widget) }
	def addToFocusStack ( widget : Widget ) { _focusedWidgetStack ::= widget }
	def popFocusStack() { _focusedWidgetStack = _focusedWidgetStack.tail }
	def removeFromFocusStack ( widget : Widget ) { _focusedWidgetStack = _focusedWidgetStack.filterNot( _ == widget ) }
	def focusedWidget = _focusedWidgetStack.headOption

	var lastFocused : Option[Widget] = None
	/**
	 * Update the focus stack, culling out any widgets that have been closed or hidden,
	 * if all widgets have been closed or hidden, we attempt to fall back on the head
	 * widget's parent, recursively, unless we bottom out at the windowing system, that
	 * way we have a marginally intelligent way of handing off focus on widget close.
	 */
	def updateFocusStack() {
		_focusedWidgetStack = _focusedWidgetStack match {
			case Nil => Nil //whatever, doesn't matter
			case head :: tail => {
				_focusedWidgetStack.map {
					w => {
						var tw = w
						while ( tw != null && (!tw.acceptsFocus || tw.closed || ! tw.showing) ) {
							tw = tw.parent.orNull
						}
						if ( tw != null ) { tw }
						else { Widget.Sentinel }
					}
				}.filterNot( _.isSentinel )
			}
		}
		if (focusedWidget != lastFocused) {
			for (lf <- lastFocused) {
				lf.handleEvent(new FocusLostEvent().withOrigin(lf))
			}
			for (nf <- focusedWidget) { nf.handleEvent(new FocusGainedEvent().withOrigin(nf)) }
		}
		lastFocused = focusedWidget
	}
}

object WindowingSystem2 extends ClipboardOwner {
	var drawingTicksDone = 0

	val conf = ResourceManager.sml("ui/WindowingSystem.conf")

	class RenderingContext(val shader: Shader,val textureBlock : TextureBlock, val screenSpaceRatios : ReadVec2f ){
		var translation = Vec3f(0.0f,0.0f,0.0f)
		def translationPixelsX = translation.x * screenSpaceRatios.x
		def translationPixelsY = translation.y * screenSpaceRatios.y
		var scale = Vec3f(1.0f,1.0f,1.0f)
		var boundsStack = List[Rectf]()
		var newOpenglWidgetIndices = List[(Int,OpenGLWidget)]()

		def translationBlock ( x : Float , y : Float ,z  : Float, sx : Float, sy : Float, sz : Float )(stmt : => Unit ) {
			if ( x != 0 || y != 0 || z != 0 || sx != 0 || sy != 0 || sz != 0 ) {
				val preX = translation.x
				val preY = translation.y
				val preZ = translation.z
				translation.x += x * scale.x
				translation.y += y * scale.y
				translation.z += z * scale.x + 0.01f

				val preScaleX = scale.x
				val preScaleY = scale.y
				val preScaleZ = scale.z
				scale.x *= sx
				scale.y *= sy
				scale.z *= sz

				stmt

				scale.x = preScaleX
				scale.y = preScaleY
				scale.z = preScaleZ

				translation.x = preX
				translation.y = preY
				translation.z = preZ
			} else {
				stmt
			}
		}
		def translationBlock ( v : Vec3f , s : Vec3f )(stmt : => Unit ) {
			translationBlock(v.x,v.y,v.z,s.x,s.y,s.z)(stmt)
		}

		def setBounds ( avbo : AVBO , attrIndex : Int , vi : Int ) {
			val bs = boundsStack.head
			avbo.setA(attrIndex,vi,bs.x,bs.y,bs.x + bs.w,bs.y + bs.h)
		}
		def setBoundsToParent(avbo: AVBO, attrIndex: Int, vi: Int){
			val bs = boundsStack.takeRight(2).head
			avbo.setA(attrIndex,vi,bs.x,bs.y,bs.x + bs.w,bs.y + bs.h)
		}

		def toPixelScaleX ( x : Float ) = x * screenSpaceRatios.x
		def toPixelScaleY ( y : Float ) = y * screenSpaceRatios.y
		def fromPixelScaleX ( x : Float ) = x / screenSpaceRatios.x
		def fromPixelScaleY ( y : Float ) = y / screenSpaceRatios.y
	}

	def clipboardText : Option[String] = {
		val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
		try {
			val contents = clipboard.getContents(this)
			if ( contents.getTransferDataFlavors.contains( DataFlavor.stringFlavor ) ) {
				contents.getTransferData( DataFlavor.stringFlavor ) match {
					case string : String => {
						return Some(string)
					}
					case other => {
						Noto.warn(f"Despite requesting a string flavored clipboard, we got $other instead")
					}
				}
			}
		} catch {
			case ise : IllegalStateException => Noto.info("System clipboard is a bit throw-happy")
			case ioe : IOException => Noto.info("System clipboard got mad because its state changed, or something, string flavor not available")
		}
		None
	}

	def copyTextToClipboard ( str : String ) {
		val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
		try {
			clipboard.setContents(new StringSelection(str),this)
		} catch {
			case ise : IllegalStateException => Noto.info("System clipboard is a bit throw-happy")
			case ioe : IOException => Noto.info("System clipboard got mad because its state changed, or something, string flavor not available")
		}
	}

	def lostOwnership (clipboard : Clipboard, transferable : Transferable ) {}

}
