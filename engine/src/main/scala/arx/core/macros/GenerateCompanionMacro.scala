package arx.core.macros

/**
  * Created with IntelliJ IDEA.
  * User: nvt
  * Date: 10/19/18
  * Time: 3:53 PM
  */

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox


//class GenerateCompanion extends StaticAnnotation {
//	def macroTransform(annottees: Any*) = macro GenerateCompanionMacro.printImpl
//}

object GenerateCompanionMacro {

	def printImpl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
		import c.universe._

		println("")
		annottees.map(_.tree).toList match {
			case classDef@ q"class $className { ..$body }" :: Nil => {
				println(s"object $className {")
				body.foreach {
					case q"var $variableName : $variableType = $initialValue" =>
						println(s"""\tval $variableName : Field[$className,$variableType] = Field("$variableName", (f) => f.$variableName, (f,$variableName) => f.$variableName = $variableName """)
				}
				println("}")
			}
			case _ => // do nothing
		}

		annottees.head
	}

	def actualImpl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
		import c.universe._

		//		val inputs : List[Tree] = annottees.map(_.tree)(collection.breakOut)
		//		inputs match {
		//			case (cd @ ClassDef(_, cName, _, _)) :: tail => {
		//
		//			}
		//		}

		val result = {
			annottees.map(_.tree).toList match {
				case classDef@q"class $className extends ..$parents { ..$body }" :: Nil => {
					val variableRedefs = body.map {
						case q"var $variableName : $variableType = $initialValue" =>
							q"""
								val $variableName = "hello";
								"""
					}
					val classNameSym = TermName(className.toString())

					val ret =
						q"""
			 object $classNameSym { ..$variableRedefs }

   ..$classDef
						 """

					//					classDef ::: moduleDefinition :: Nil
					ret

					//					for (bodyPart <- body) {
					//						bodyPart match {
					//							case q"var $variableName : $variableType = $initialValue" => {
					//								println(s"Variable name: $variableName, type: $variableType, initialValue: $initialValue")
					//							}
					//						}
					//					}
				}
				//				case q"$mods def $methodName[..$tpes](...$args): $returnType = { ..$body }" :: Nil => {
				//					q"""$mods def $methodName[..$tpes](...$args): $returnType =  {
				//            val start = System.nanoTime()
				//            val result = {..$body}
				//            val end = System.nanoTime()
				//            println(${methodName.toString} + " elapsed time: " + (end - start) + "ns")
				//            result
				//          }"""
				//				}
				//				case _ => c.abort(c.enclosingPosition, "Annotation @Benchmark can be used only with methods")
			}
		}

		c.Expr[Any](result)

		//		annottees.head
	}
}
