package arx.engine.control.components.windowing.widgets

import arx.Prelude._
import arx.application.Noto
import arx.core.Moddable
import arx.core.representation.ConfigValue
import arx.core.vec.{ReadVec2f, ReadVec4f, Vec2T, Vec2f, Vec2i, Vec4f}
import arx.graphics.Image
import arx.core.vec.Cardinals._
import arx.engine.EngineCore
import arx.engine.control.components.windowing.events.{FocusLostEvent, RequestFocusEvent}
import arx.engine.control.components.windowing.{Widget, WindowingSystem}
import arx.engine.control.components.windowing.widgets.TextEditorWidget.{CursorMoved, EditorOperation, SelectionMarkerChanged, TextDeleted, TextInserted}
import arx.engine.control.components.windowing.widgets.data.EventHandlingData
import arx.engine.control.event.Event.{CharEnteredEvent, KeyPressEvent, MouseDragEvent, MousePressEvent, TextInputCancel, TextInputChanged, TextInputCursorMoved, TextInputEnter}
import arx.graphics.helpers.{RichText, TextSection}
import org.lwjgl.glfw.GLFW
import scalaxy.loops._

import scala.collection.immutable.Stack

class TextEditorWidget ( parentis : Widget ) extends Widget(parentis) {
	width = DimensionExpression.WrapContent
	height = DimensionExpression.WrapContent

	protected var internalText = new StringBuilder
	var singleLine = false
	var autoIndent = false
	var onDemandEditor = false
	this.auxData[EventHandlingData].acceptsFocus = true

	var promptText = ""
	var promptColor : Moddable[ReadVec4f] = Moddable(Vec4f(0.5f,0.5f,0.6f,1.0f))

	protected var cursor = 0 //Cursor is before the element at this index
	protected var richTextCursor = 0 // cursor index before adjusting for rich text
	protected var selectionMarker : Option[Int] = None
	protected var undoStack = Stack[EditorOperation]()
	protected var redoStack = Stack[EditorOperation]()

	drawing.backgroundImage = Some("ui/minimalistBorderWhite_ne.png")
	drawing.interiorPadding = Vec2i(4,4)

	val textDisplay = new TextDisplayWidget(this)
	textDisplay.dimensions = Vec2T(DimensionExpression.Intrinsic, DimensionExpression.Intrinsic)
	textDisplay.text = Moddable(() => richTextFromRaw(this.text))

	val promptTextDisplay = new TextDisplayWidget(this)
	promptTextDisplay.dimensions = Vec2T(DimensionExpression.Intrinsic, DimensionExpression.Intrinsic)
	promptTextDisplay.text = Moddable(() => RichText(TextSection(promptText, promptColor.resolve(), None) :: Nil))
	promptTextDisplay.x = PositionExpression.Relative(textDisplay, 1)


	def text = internalText.toString()
	protected def richTextFromRaw(str : String) = RichText(str)
	protected def rawIndexFromRichTextIndex(index : Int) = index

	def setText (str : String, suppressEvent: Boolean = false): Unit = {
		internalText.clear()
		internalText.append(str)
		if (!suppressEvent) {
			handleEvent(TextInputChanged(text))
		}
	}

	def setTextPrompt(str : String) : Unit = {
		promptText = str
	}

	def cursorShowing = if (onDemandEditor) { hasFocus } else { true }
	def cursorIndex = cursor

	def continueEditorOperation[T <: TextEditorWidget.EditorOperation : Manifest](orElse : => T) : T = {
		undoStack.headOption match {
			case Some(top) if top.getClass == manifest[T].runtimeClass =>
			case None => pushUndoOperation(orElse)
			case _ => pushUndoOperation(orElse)
		}
		undoStack.head.asInstanceOf[T]
	}
	def clearRedoStack () { redoStack = Stack() }
	def pushUndoOperation ( op : EditorOperation ) {
		undoStack = undoStack push op
		clearRedoStack()
	}

	def currentText = internalText.toString()
	
	def clear () {
		internalText.clear()
		promptText = ""
		cursor = 0
		selectionMarker = None
		undoStack = Stack()
		redoStack = Stack()
	}

	def selectAll (): Unit = {
		selectionMarker = Some(0)
		moveCursorTo(currentText.length)
	}

	def selectedRange = selectionMarker match {
		case Some(marker) => if ( marker > cursor ) {
			Some((cursor,marker))
		} else if ( marker < cursor ) {
			Some((marker,cursor))
		} else {
			None
		}
		case None => None
	}

	def moveCursorTo ( index : Int , createUndoStep : Boolean = false ) {
		val cursorDest = clamp(index,0,internalText.size)
		if ( createUndoStep ) {
			val operation = continueEditorOperation(new CursorMoved(cursor,cursorDest))
			operation.destCursorIndex = index
		}

		cursor = cursorDest
		handleEvent( TextInputCursorMoved(cursor) )
	}
	def setSelectionMarker ( indexRaw : Int , createUndoStep : Boolean = true ) {
		if ( selectionMarker.isEmpty ) {
			if ( createUndoStep ) {
				pushUndoOperation(new SelectionMarkerChanged(indexRaw,-1))
			}
			selectionMarker = Some(indexRaw)
		}
	}
	def clearSelectionMarker (createUndoStep : Boolean = true ) {
		if ( createUndoStep && selectionMarker.nonEmpty ) {
			pushUndoOperation(new SelectionMarkerChanged(-1,selectionMarker.get))
		}
		selectionMarker = None
	}
	def updateSelectionMarker ( clear : Boolean ) {
		if ( clear ) { clearSelectionMarker() }
		else { setSelectionMarker(cursor) }
	}

	def insertStr(str : String, createUndoSteps : Boolean = true ) {
		if (str.isEmpty) { return }

		if ( selectionMarker.nonEmpty ) { deleteSelection() }
		internalText = internalText.insert(cursor,str)
		handleEvent ( TextInputChanged(text) )

		if ( createUndoSteps ) {
			val operation = continueEditorOperation(new TextInserted(cursor))
			operation.text += str
			if ( ! Character.isLetterOrDigit(str.charAt(0)) ) {
				pushUndoOperation(new TextInserted(cursor+1))
			}
			clearRedoStack()
		}
	}

	def deleteSelection () {
		val (start,end) = selectedRange.getOrElse( (cursor-1,cursor) )
		if ( start >= 0 && end >= 0 ) {
			val deletedText = internalText.substring(start,end)
			for ( i <- start until end ; if i >= 0 ) {
				internalText = internalText.deleteCharAt(start)
			}
			val operation = continueEditorOperation(new TextDeleted(start,"",cursor))
			operation.startIndex = start
			operation.deletedText = deletedText + operation.deletedText
			if ( selectedRange.nonEmpty ) { pushUndoOperation(new TextDeleted(start,"",cursor)) }

			moveCursorTo(start)
			clearSelectionMarker()
			clearRedoStack()
		}
	}

	def undo () {
		var break = false
		while ( undoStack.nonEmpty && (! break || undoStack.top.freestep) ) {
			val top = undoStack.top
			break = top.undo(this)
			redoStack = redoStack push top
			undoStack = undoStack.pop
		}
	}
	def redo () {
		var break = false
		while ( redoStack.nonEmpty && (! break || redoStack.top.freestep) ) {
			val top = redoStack.top
			break = top.redo(this)
			undoStack = undoStack push top
			redoStack = redoStack.pop
		}
	}

	protected def intersectedIndex(rawAbsolutePos : ReadVec2f) : Int = {
		val absolutePos = rawAbsolutePos * EngineCore.pixelScaleFactor

		val renderedData = textDisplay.auxData[TextDisplayRenderedGlyphData]
		val rects = renderedData.glyphRects
		if (rects.isEmpty || rects.head.y > absolutePos.y) {
			-1
		} else {
			var minDist = 10000000.0f
			var minDistIndex = rects.size
			for (i <- 0 until rects.size optimized) {
				val rect = rects(i).translate(renderedData.absoluteOffset)
				if (absolutePos.y >= rect.y && absolutePos.y <= rect.maxY) {
					val dist1 = (absolutePos.x - rect.minX).abs
					if (dist1 < minDist) {
						minDist = dist1
						minDistIndex = i
					}

					val dist2 = (absolutePos.x - rect.maxX).abs
					if (dist2 < minDist) {
						minDist = dist2
						minDistIndex = i + 1
					}
				}
				// TODO: Short circuit if past line we need
			}
			minDistIndex
		}
	}

	consumeEvent {
		case mde : MouseDragEvent => {
			val hitIndex = intersectedIndex(mde.mousePos.xy)
			updateSelectionMarker( clear = false )
			moveCursorTo(hitIndex,createUndoStep = true)
		}
		case mpe : MousePressEvent => {
			val hitIndex = intersectedIndex(mpe.mousePos.xy)
			updateSelectionMarker( ! mpe.modifiers.shift )
			moveCursorTo(hitIndex,createUndoStep = true)
		}
		case kpe : KeyPressEvent => {
			kpe.key match {
				case GLFW.GLFW_KEY_LEFT => {
					val moveTo = if (kpe.modifiers.shift) {
						cursor-1
					} else {
						math.min(cursor,selectionMarker.getOrElse(Int.MaxValue))-1
					}
					updateSelectionMarker( ! kpe.modifiers.shift )
					moveCursorTo(moveTo,createUndoStep = true)
				}
				case GLFW.GLFW_KEY_RIGHT => {
					val moveTo = math.max(cursor,selectionMarker.getOrElse(-1))+1
					updateSelectionMarker( ! kpe.modifiers.shift )
					moveCursorTo(moveTo,createUndoStep = true)
				}
//				case GLFW.GLFW_KEY_DOWN => {
//					updateSelectionMarker( ! kpe.modifiers.shift )
//					val currentPos = editorRenderer.cursorPosition
//					val targetPos = currentPos + Vec2f(0.0f, textRenderer.lineHeight * 1.5f)
//					moveCursorTo( textRenderer.intersectedIndex(targetPos) ,createUndoStep = true)
//				}
//				case GLFW.GLFW_KEY_UP => {
//					updateSelectionMarker( ! kpe.modifiers.shift )
//					val currentPos = editorRenderer.cursorPosition
//					val targetPos = currentPos - Vec2f(0.0f, textRenderer.lineHeight * 0.5f)
//					moveCursorTo( textRenderer.intersectedIndex(targetPos) ,createUndoStep = true)
//				}
				case GLFW.GLFW_KEY_DELETE | GLFW.GLFW_KEY_BACKSPACE => {
					deleteSelection()
				}
				case GLFW.GLFW_KEY_V if kpe.modifiers.ctrl => {
					if ( selectionMarker.nonEmpty ) { deleteSelection() }
					WindowingSystem.clipboardText match {
						case Some(str) =>
							insertStr(str)
							moveCursorTo(cursor + 1)
						case None =>
					}
				}
				case GLFW.GLFW_KEY_C if kpe.modifiers.ctrl => {
					selectedRange match {
						case Some((start,end)) => WindowingSystem.copyTextToClipboard( currentText.substring(start,end) )
						case _ =>
					}
				}
				case GLFW.GLFW_KEY_X if kpe.modifiers.ctrl => {
					selectedRange match {
						case Some((start,end)) => {
							WindowingSystem.copyTextToClipboard( currentText.substring(start,end) )
							deleteSelection()
						}
						case _ =>
					}
				}
				case GLFW.GLFW_KEY_A if kpe.modifiers.ctrl => selectAll()
				case GLFW.GLFW_KEY_Z if kpe.modifiers.ctrl && ! kpe.modifiers.shift => undo()
				case GLFW.GLFW_KEY_Z if kpe.modifiers.ctrl && kpe.modifiers.shift => redo()
				case GLFW.GLFW_KEY_Y if kpe.modifiers.ctrl => redo()
				case GLFW.GLFW_KEY_ENTER if singleLine && onDemandEditor => {
					handleEvent(TextInputEnter(text))
					handleEvent(RequestFocusEvent(parent))
				}
				case GLFW.GLFW_KEY_ESCAPE if singleLine && onDemandEditor => {
					handleEvent(TextInputCancel(text))
					handleEvent(RequestFocusEvent(parent))
				}
				case GLFW.GLFW_KEY_ENTER if singleLine || kpe.modifiers.ctrl => {
					handleEvent( TextInputEnter(text) )
				}
				case GLFW.GLFW_KEY_ENTER if ! singleLine => {
					insertStr("\n")
					moveCursorTo( cursor + 1 )
					if ( autoIndent ) {
						var previousNewlineIndex = cursor - 2
						while ( previousNewlineIndex > 0 && internalText(previousNewlineIndex) != '\n' ) { previousNewlineIndex -= 1 }
						var tabCount = 0
						previousNewlineIndex += 1
						while ( previousNewlineIndex <= cursor - 2 && internalText(previousNewlineIndex) == '\t' ) { tabCount += 1; previousNewlineIndex += 1 }
						for ( t <- 0 until tabCount ) {
							insertStr("\t")
							moveCursorTo(cursor + 1)
						}
					}
				}
				case GLFW.GLFW_KEY_TAB if ! singleLine => { //tabs don't make sense for single line text editors really
					insertStr("\t")
					clearSelectionMarker()
					moveCursorTo( cursor + 1 )
				}
//				case _ if kpe.asciiChar >= ' ' && kpe.asciiChar <= '~' => {
//					insertStr(kpe.asciiChar)
//					clearSelectionMarker()
//					moveCursorTo( cursor + 1 )
//				}
				case _ => false
			}
		}
		case CharEnteredEvent(str) =>
			insertStr(str)
			clearSelectionMarker()
			moveCursorTo( cursor + 1 )
	}

	onEvent {
		case fle : FocusLostEvent => clearSelectionMarker()
	}

//	override protected[gui2] def SMLTypeIdentifier = "text editor"
//
//	//+====================+ SML Interface +====================+
//	override def setFromSML(sml: ConfigValue,overwrite:Boolean) = {
//		if ( overwrite && sml.text.nonEmpty ) { internalText = new StringBuilder( sml.text.str ) }
//		autoIndent = sml.autoIndent.boolOrElse(autoIndent)
//		singleLine = sml.singleLine.boolOrElse(singleLine)
//		onDemandEditor = sml.onDemandEditor.boolOrElse(onDemandEditor)
//		super.setFromSML (sml,overwrite)
//
//	}
}

object TextEditorWidget {
	abstract class EditorOperation{
		def freestep : Boolean = false
		def undo ( editor : TextEditorWidget ) : Boolean
		def redo ( editor : TextEditorWidget ) : Boolean
	}

	class TextInserted(startIndex : Int) extends EditorOperation {
		var text : String = ""
		def undo(editor: TextEditorWidget) = {
			editor.internalText = editor.internalText.delete(startIndex,startIndex + text.length)
			editor.moveCursorTo(startIndex)
			text.nonEmpty
		}
		def redo(editor: TextEditorWidget) = {
			editor.internalText = editor.internalText.insert(startIndex,text)
			editor.moveCursorTo(startIndex + text.length)
			text.nonEmpty
		}
	}
	class TextDeleted ( var startIndex : Int , var deletedText : String , initialCursorIndex : Int ) extends EditorOperation {
		def undo(editor: TextEditorWidget) = {
			editor.internalText = editor.internalText.insert(startIndex,deletedText)
			editor.moveCursorTo(initialCursorIndex)
			deletedText.nonEmpty
		}
		def redo(editor: TextEditorWidget) = {
			editor.internalText = editor.internalText.delete(startIndex,startIndex + deletedText.length)
			editor.moveCursorTo(startIndex)
			deletedText.nonEmpty
		}
	}
	case class SelectionMarkerChanged ( selectionMarker : Int , oldSelectionMarker : Int ) extends EditorOperation {
		override def freestep = true
		def undo(editor: TextEditorWidget) = {
			if ( oldSelectionMarker == -1 ) { editor.clearSelectionMarker(createUndoStep = false) }
			else { editor.setSelectionMarker(oldSelectionMarker,createUndoStep = false) }
			oldSelectionMarker == -1
		}
		def redo(editor: TextEditorWidget) = {
			if ( selectionMarker == -1 ) { editor.clearSelectionMarker(createUndoStep = false) }
			else { editor.setSelectionMarker(selectionMarker,createUndoStep = false) }
			selectionMarker == -1
		}
	}
	class CursorMoved ( var initialCursorIndex : Int, var destCursorIndex : Int ) extends EditorOperation {
		def undo(editor: TextEditorWidget) = {
			editor.cursor = initialCursorIndex
			initialCursorIndex != destCursorIndex
		}
		def redo(editor: TextEditorWidget) = {
			editor.cursor = destCursorIndex
			initialCursorIndex != destCursorIndex
		}
	}
}

class TextInputWidget(parentis : Widget) extends TextEditorWidget(parentis) {
	singleLine = true
	onDemandEditor = true

//	override protected[gui2] def SMLTypeIdentifier: String = "text input"
}