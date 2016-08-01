package arx.core.repl

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 9/19/13
 * Time: 11:24 AM
 * To change this template use File | Settings | File Templates.
 */

import java.io.PrintWriter
import java.io.Writer
import java.lang.reflect.Modifier

import arx.application.Noto
import arx.core.introspection.ReflectionAssistant

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.interpreter.NamedParam
import scala.tools.nsc.interpreter.Results

class ArxInterpreter(sw : Writer) {
	protected val mw = new MuteableWriter(sw)
	protected val pw = new PrintWriter(mw)

	protected val settings = new Settings()
	settings.usejavacp.value = true
	protected val main = new IMain(settings,pw)
	addImport("arx.Prelude._")

	protected val classHolder = new ClassHolder
	protected val classHolderParameter = NamedParam("_classHolder",classHolder)

	main.bind(classHolderParameter)

	protected var resCount = 0
	protected var reifCounter = -1

	Noto.listeners ::= notoListener _
	protected def notoListener ( str : String , level : Int ) {
		sw.append(str)
	}

	def bind[T:Manifest] ( name : String, value : T ) {
		main.bind(NamedParam(name,value))
	}

	def smartImport ( className : String ) = {
		import scala.collection.JavaConversions._
		ReflectionAssistant.reflections.getAllTypes.find ( _.endsWith("." + className) ) match {
			case Some(cinfo) => {
				main.interpret("import " + cinfo)
			}
			case None =>
				Noto.warn("No smart import found for : " + className)
				sw.append("No smart import found for : " + className)
		}
	}
	def addImport ( className : String ) = {
		main.interpret("import " + className)
	}


	def interpret ( baseString : String ) : String = {
		var funcString = baseString.trim

		var counter = -1
		while ( funcString.indexOf('`',counter+1) != -1 ) {
			counter = funcString.indexOf('`',counter+1)
			val typeString = funcString.substring(counter+1).takeWhile( _.isLetterOrDigit )
			smartImport(typeString)
		}
		funcString = funcString.replace('`',' ')

		var execString = funcString

		var tmpVariable : Option[String] = None
		val startingResCount = resCount
		if ( ! funcString.startsWith("val ") && ! funcString.startsWith("var ") && ! funcString.startsWith("def") && ! funcString.contains(" = ") ) {
//			val baseFString = funcString
			tmpVariable = Some(f"$$tmp$resCount")
			execString = f"val $$tmp$resCount = $funcString"
			funcString = f"val res$resCount = $funcString"
			resCount += 1
		}



		sw.append("> " + funcString + "\n")
		main.interpret(execString) match {
			case Results.Success =>
				tmpVariable match {
					case Some(tmpVarName) => {
						mw.muted = true
						main.interpret(f"_classHolder.clazz = ($tmpVarName).getClass")
						val effClassName = classHolder.clazz.getCanonicalName match {
							case "int" => "Int"
							case "float" => "Float"
							case "byte" => "Byte"
							case "short" => "Short"
							case "boolean" => "Boolean"
							case "long" => "Long"
							case "double" => "Double"
							case other => {
								classHolder.clazz.getTypeParameters.length match {
									case 0 => other
									case ntp => "void"//other + "[" + (0 until ntp).map(i => "_").reduceLeft(_ + "," + _) + "]"
								}
							}
						}
						mw.muted = false

						val fudgeTo = effClassName match {
							case "java.lang.Integer" => ": Int"
							case "java.lang.Float" => ": Float"
							case "java.lang.Double" => ": Double"
							case "java.lang.Boolean" => ": Boolean"
							case "java.lang.Short" => ": Short"
							case "java.lang.Long" => ": Long"
							case "java.lang.Byte" => ": Byte"
							case _ => ""
						}

						if ( Modifier.isPublic(classHolder.clazz.getModifiers) && effClassName != "void" && effClassName != "null" && effClassName != null && ! effClassName.endsWith("$") ) {
							main.interpret(f"val res$startingResCount = $tmpVarName.asInstanceOf[$effClassName] $fudgeTo")
						} else {
							main.interpret(f"val res$startingResCount = $tmpVarName")
						}
					}
					case _ =>
				}
			case _ => Noto.warn("Interpreter failed to execute")
		}
		sw.toString
	}

	def reify ( baseString : String ) = {
		reifCounter += 1
		interpret("_classHolder.clazz = (" + baseString + ").getClass")
		if ( ! baseString.contains(".") ) {
			interpret("val tmp = (" + baseString + ").asInstanceOf[" + classHolder.clazz.getCanonicalName + "]")
			interpret("val " + baseString + " : " + classHolder.clazz.getCanonicalName + " = tmp")
		} else {
			interpret("val reif" + reifCounter + " = (" + baseString + ").asInstanceOf[" + classHolder.clazz.getCanonicalName + "]")
		}
	}
}

class MuteableWriter(delegateTo:Writer) extends Writer {
	var muted = false

	def write(p1: Array[Char], p2: Int, p3: Int) {
		if (! muted){
			if ( p1.length > 0 && p1(0) != '$' ) {
				delegateTo.write(p1,p2,p3)
			}
		}
	}

	def flush() {
		delegateTo.flush()
	}

	def close() {
		delegateTo.close()
	}
}

class ClassHolder { var clazz : Class[_] = this.getClass }