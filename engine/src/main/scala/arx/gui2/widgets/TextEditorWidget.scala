package arx.gui2.widgets

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 10/4/13
 * Time: 12:39 PM
 * To change this template use File | Settings | File Templates.
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.Moddable
import arx.core.representation.ConfigValue
import arx.core.vec.Vec2f
import arx.graphics.Image
import arx.gui2.events.FocusLostEvent
import arx.gui2.rendering.TextEditorWidgetRenderingComponent
import arx.gui2.widgets.TextEditorWidget._
import arx.gui2.Widget
import arx.gui2.WindowingSystem2
import arx.core.ImplicitModdable._
import arx.gui2.events._
import arx.core.vec.Cardinals._
import arx.engine.control.event.Event._
import org.lwjgl.glfw.GLFW

import scala.collection.immutable.Stack

class TextEditorWidget ( parentis : Widget ) extends TextDisplayWidget(parentis) {
	protected var internalText = new StringBuilder
	var singleLine = false
	var autoIndent = false
	var onDemandEditor = false
	acceptsFocus = true

	protected var cursor = 0 //Cursor is before the element at this index
	protected var selectionMarker : Option[Int] = None
	protected var undoStack = Stack[EditorOperation]()
	protected var redoStack = Stack[EditorOperation]()

	val editorRenderer = new TextEditorWidgetRenderingComponent(textRenderer,cursor _,selectedRange  _,shouldShowCursor _)
	renderers = renderers.head :: editorRenderer :: renderers.tail
	pixelScale = 2

	def setInitialText (str : String): Unit = {
		internalText.clear()
		internalText.append(str)
	}

	protected def shouldShowCursor = if (onDemandEditor) {
		hasFocus
	} else { true }

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
	override def text = currentText _
	override def text_= ( t : Moddable[String] ) {
		Noto.warn("Do not attempt to set the text of a text editor widget directly, please")
	}
	def clear () {
		internalText.clear()
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

	def insertChar ( char : Character, createUndoSteps : Boolean = true ) {
		if ( selectionMarker.nonEmpty ) { deleteSelection() }
		internalText = internalText.insert(cursor,char)
		fireEvent ( TextInputChanged(text,None,None) )

		if ( createUndoSteps ) {
			val operation = continueEditorOperation(new TextInserted(cursor))
			operation.text += char
			if ( ! Character.isLetterOrDigit(char) ) {
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

	consumeEvent {
		case mde : MouseDragEvent => {
			val relativePos = mde.mousePos.xy - this.absolutePosition.xy
			val hitIndex = textRenderer.intersectedIndex(relativePos)
			updateSelectionMarker( clear = false )
			moveCursorTo(hitIndex,createUndoStep = true)
		}
		case mpe : MousePressEvent => {
			val relativePos = mpe.mousePos.xy - this.absolutePosition.xy
			val hitIndex = textRenderer.intersectedIndex(relativePos)
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
				case GLFW.GLFW_KEY_DOWN => {
					updateSelectionMarker( ! kpe.modifiers.shift )
					val currentPos = editorRenderer.cursorPosition
					val targetPos = currentPos + Vec2f(0.0f, textRenderer.lineHeight * 1.5f)
					moveCursorTo( textRenderer.intersectedIndex(targetPos) ,createUndoStep = true)
				}
				case GLFW.GLFW_KEY_UP => {
					updateSelectionMarker( ! kpe.modifiers.shift )
					val currentPos = editorRenderer.cursorPosition
					val targetPos = currentPos - Vec2f(0.0f, textRenderer.lineHeight * 0.5f)
					moveCursorTo( textRenderer.intersectedIndex(targetPos) ,createUndoStep = true)
				}
				case GLFW.GLFW_KEY_DELETE | GLFW.GLFW_KEY_BACKSPACE => {
					deleteSelection()
				}
				case GLFW.GLFW_KEY_V if kpe.modifiers.ctrl => {
					if ( selectionMarker.nonEmpty ) { deleteSelection() }
					WindowingSystem2.clipboardText match {
						case Some(str) => for ( char <- str ) {
							insertChar(char)
							moveCursorTo(cursor + 1)
						}
						case None =>
					}
				}
				case GLFW.GLFW_KEY_C if kpe.modifiers.ctrl => {
					selectedRange match {
						case Some((start,end)) => WindowingSystem2.copyTextToClipboard( currentText.substring(start,end) )
						case _ =>
					}
				}
				case GLFW.GLFW_KEY_X if kpe.modifiers.ctrl => {
					selectedRange match {
						case Some((start,end)) => {
							WindowingSystem2.copyTextToClipboard( currentText.substring(start,end) )
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
					fireEvent(TextInputEnter(text))
					for (p <- parent) { windowingSystem.giveFocusTo(p) }
				}
				case GLFW.GLFW_KEY_ESCAPE if singleLine && onDemandEditor => {
					fireEvent(TextInputCancel(text))
					for (p <- parent) { windowingSystem.giveFocusTo(p) }
				}
				case GLFW.GLFW_KEY_ENTER if singleLine || kpe.modifiers.ctrl => {
					fireEvent( TextInputEnter(text) )
				}
				case GLFW.GLFW_KEY_ENTER if ! singleLine => {
					insertChar('\n')
					moveCursorTo( cursor + 1 )
					if ( autoIndent ) {
						var previousNewlineIndex = cursor - 2
						while ( previousNewlineIndex > 0 && internalText(previousNewlineIndex) != '\n' ) { previousNewlineIndex -= 1 }
						var tabCount = 0
						previousNewlineIndex += 1
						while ( previousNewlineIndex <= cursor - 2 && internalText(previousNewlineIndex) == '\t' ) { tabCount += 1; previousNewlineIndex += 1 }
						for ( t <- 0 until tabCount ) {
							insertChar('\t')
							moveCursorTo(cursor + 1)
						}
					}
				}
				case GLFW.GLFW_KEY_TAB if ! singleLine => { //tabs don't make sense for single line text editors really
					insertChar('\t')
					clearSelectionMarker()
					moveCursorTo( cursor + 1 )
				}
				case _ if kpe.asciiChar >= ' ' && kpe.asciiChar <= '~' => {
					insertChar(kpe.asciiChar)
					clearSelectionMarker()
					moveCursorTo( cursor + 1 )
				}
				case _ => false
			}
		}
	}

	onEvent {
		case fle : FocusLostEvent => clearSelectionMarker()
	}

	override protected[gui2] def SMLTypeIdentifier = "text editor"

	//+====================+ SML Interface +====================+
	override def setFromSML(sml: ConfigValue,overwrite:Boolean) = {
		if ( overwrite && sml.text.nonEmpty ) { internalText = new StringBuilder( sml.text.str ) }
		autoIndent = sml.autoIndent.boolOrElse(autoIndent)
		singleLine = sml.singleLine.boolOrElse(singleLine)
		onDemandEditor = sml.onDemandEditor.boolOrElse(onDemandEditor)
		super.setFromSML (sml,overwrite)

	}
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

class TextInput(parentis : Widget) extends TextEditorWidget(parentis) {
	singleLine = true
	onDemandEditor = true
	textAlignment = Center

	override protected[gui2] def SMLTypeIdentifier: String = "text input"
}

class TextControl (parentis : Widget) extends Widget(parentis) {
	val label = new TextDisplayWidget(this)
	label.backgroundImage = Image.Sentinel
	label.matchTextDimensions()
	label.centerVertically()
	label.x = 0.5f
	val input = new TextInput(this)
	input.pixelScale = 1
	input.width = () => this.clientWidth - input.x - 0.5f
	input.centerVertically()

	input.dockRight(label,0.5f)

	def currentValue = input.currentText

	override protected[gui2] def SMLTypeIdentifier: String = "text control"

	//+====================+ SML Interface +====================+
	override def setFromSML(sml: ConfigValue, overwrite: Boolean): Unit = {
		if (sml.label.nonEmptyValue) { label.text = sml.label.str }
		if (sml.initialValue.nonEmptyValue) { input.setInitialText(sml.initialValue.str) }

		super.setFromSML (sml, overwrite)
	}
}