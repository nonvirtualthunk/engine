package arx.core.vec

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 12/16/12
 * Time: 2:07 PM
 * Created by nonvirtualthunk
 */

import java.io.{FileWriter, FileOutputStream, File}

//class ReadVec3i extends InternVec3i {
//	def this ( xa : Int , ya : Int , za : Int ) { this(); xi = xa; yi = ya; zi = za; }
//
//	def x = xi
//	def y = yi
//	def z = zi
//
//	def + ( v : ReadVec3i ) : Vec3i = new ReadVec3i(xi,yi,zi)
//}
//class Vec3i extends ReadVec3i {
//
//}
//object ReadVec3i {
//	def apply ( xi : Int , yi : Int , zi : Int ) = {
//		val v = new ReadVec3i
//	}
//}

//class ReadVec#N##t# extends InternVec#N##t# {
//
//}

object VectorGenerator {
	case class ValueType ( name : String , javaName : String , char : String , defaultValue : String , floatingPoint : Boolean )
	case class Output ( fileName : String , classContent : String )

	val AccessModifiers = List("Read","","Intern")
	val Arities = List(2,3,4)

	val IntValueType = ValueType("Int","int","i","0",floatingPoint = false)
	val FloatValueType = ValueType("Float","float","f","0.0f",floatingPoint = true)
	val ValueTypes = List( IntValueType , FloatValueType )//, ValueType("Byte","byte","b","0") )

	val varNamesByArity = Map( 2 -> List("x","y") , 3 -> List("x","y","z") , 4 -> List("r","g","b","a") )
	var alternateNamesByArity = Map ( 3 -> List("r","g","b") )

	val operations = List("+","-","*","/")
	val extraOperationsByType = Map( IntValueType -> List("%",">>","<<") )

	def classNameFor(mod:String,ar:Int,vtype:ValueType) = mod + "Vec" + ar + vtype.char
	val outer = this


	def generate = {
		for ( accessModifier <- AccessModifiers ; arity <- Arities ; valueType <- ValueTypes ) yield {
			var classString = ""
			def classNameForAccessModifier(m:String) = m + "Vec" + arity + valueType.char
			def classNameFor(mod:String,ar:Int) = outer.classNameFor(mod,ar,valueType)
			val className = classNameForAccessModifier(accessModifier)
			val varNames = varNamesByArity(arity)
			val varNamesi = varNames.map(_ + "i")

			if ( accessModifier == "Intern" ) {
				classString += "import java.io.Externalizable;\n" +
									"import java.io.IOException;\n" +
									"import java.io.ObjectInput;\n" +
									"import java.io.ObjectOutput;\n\n"
				classString += "public class " + className + " implements Externalizable{\n"
				for ( varName <- varNames ) {
					classString += "\tprotected " + valueType.javaName + " " + varName + "i;\n"
				}
				classString += "\tprivate static final long serialVersionUID = 9223372036854770000L;\n"
				classString += "\tpublic " + className + "(){}\n"
				classString += "\tpublic " + className + "(" + varNames.map(n => valueType.javaName + " " + n + "a").reduceLeft(_ + ", " + _) + ") {\n"
				classString += varNames.map(n => "\t\t" + n + "i = " + n + "a;\n").reduceLeft(_ + _)
				classString += "\t}\n"

				classString += "\t@Override\n\tpublic void writeExternal(ObjectOutput out) throws IOException {\n" +
										varNames.map(n => "\t\tout.write" + valueType.name + "(" + n + "i);\n").reduceLeft(_ + "\n" + _) + "\n\t}\n\n"
				classString += "\t@Override\n\tpublic void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {\n" +
										varNames.map(n => "\t\t" + n + "i = in.read" + valueType.name + "();").reduceLeft(_ + "\n" + _) +
									"\n\t}\n"

				classString += "}\n"
			} else {
				val argString = "(" + varNames.map(n => n + "a : " + valueType.name).reduceLeft(_ + "," + _) + ")"

				if ( accessModifier == "Read" ) {
					classString += "import arx.core.Moddable\n"
					classString += "import arx.core.SelfModdable\n"
				}

				classString += "@SerialVersionUID(9223372036854770000L)\n"
				classString += "class " + className
				if ( accessModifier == "Read" ) { classString += " extends " + classNameForAccessModifier("Intern") + " with SelfModdable[" + className + "]" }
				else if ( accessModifier == "" ) { classString += " extends " + classNameForAccessModifier("Read") }
				classString += "{\n"

				classString += "\tdef this" + argString + "{ \n\t\tthis()\n" + varNames.map(n => "\t\t" + n + "i = " + n + "a" + "\n").reduceLeft(_ + _) + "\t}\n"

				if ( accessModifier == "Read" ) {
					val writeClassName = classNameForAccessModifier("")
					for ( op <- (operations ::: extraOperationsByType.getOrElse(valueType,Nil)) ) {
						classString += "\tdef " + op + "(m : Moddable[" + className + "]) = { val v = m.resolve(); new " + className + "(" + varNames.map(n => n + "i " + op + " v." + n + "i").reduceLeft(_ + "," + _) + ") }\n"
						classString += "\tdef " + op + "(v : " + className + ") = new " + className + "(" + varNames.map(n => n + "i " + op + " v." + n + "i").reduceLeft(_ + "," + _) + ")\n"
						classString += "\tdef " + op + "(s : " + valueType.name + ") = new " + className + "(" + varNames.map(n => n + "i " + op + " s").reduceLeft(_ + "," + _) + ")\n"
					}

					for ( varName <- varNames ) {
						classString += "\tdef " + varName + " = " + varName + "i\n"
						//TODO: add plusX, minusX, plusY, etc.
//						classString += "\tdef plus" + varName.capitalize + " ( f : " +
						classString += "\tprotected def " + varName + "_= ( s : " + valueType.name + " ) { " + varName + "i = s }\n"
					}
					for ( (altVarName,varName) <- alternateNamesByArity.getOrElse(arity,Nil).zip(varNames) ) {
						classString += "\tdef " + altVarName + " = " + varName + "i\n"
						classString += "\tprotected def " + altVarName + "_= ( s : " + valueType.name + " ) { " + varName + "i = s }\n"
					}

					for ( subArity <- (2 until arity) ; varNameCombinations <- varNames.combinations(subArity) ; varNamePermutations <- varNameCombinations.permutations ) {
						classString += "\tdef " + varNamePermutations.reduceLeft(_ + _) + " = new " + classNameFor("",subArity) + "(" + varNamePermutations.map(_ +"i").reduceLeft(_ + "," + _) + ")\n"
					}

					val euclid = "\t\tval e = " + varNamesi.map(n => n + "*" + n).reduceLeft(_ + " + " + _) + "\n"
					classString += "\tdef length : Float = {\n" +
										euclid +
										"\t\tmath.sqrt(e).toFloat\n" +
										"\t}\n"
					classString += "\tdef lengthSafe : Float = {\n" +
										euclid +
										"\t\tif ( e != 0 ) { math.sqrt(e).toFloat } else { 0 }\n" +
										"\t}\n"
					classString += "\tdef abs = new " + writeClassName + "(" + varNames.map(n => "math.abs(" + n + ")").reduceLeft(_ +"," + _) + ")\n"
					classString += "\tdef min(v:" + className + ") = new " + writeClassName + "(" + varNames.map(n => "math.min(" + n + ", v." + n + ")").reduceLeft(_ +"," + _) + ")\n"
					classString += "\tdef max(v:" + className + ") = new " + writeClassName + "(" + varNames.map(n => "math.max(" + n + ", v." + n + ")").reduceLeft(_ +"," + _) + ")\n"
					classString += "\tdef minMax(v:" + className + ") = ( min(v) , max(v) )\n"
					classString += "\tdef min = " + (varNames.reduceLeft[String]{ case (sum:String,varName) => "math.min(" + sum + "," + varName + ")" }) + "\n"
					classString += "\tdef max = " + (varNames.reduceLeft[String]{ case (sum:String,varName) => "math.max(" + sum + "," + varName + ")" }) + "\n"
					classString += "\tdef ceil = new " + writeClassName + "(" + varNames.map(n => "math.ceil(" + n + ").to" + valueType.name).reduceLeft(_ +"," + _) + ")\n"
					classString += "\tdef floor = new " + writeClassName + "(" + varNames.map(n => "math.floor(" + n + ").to" + valueType.name).reduceLeft(_ +"," + _) + ")\n"
					classString += "\toverride def toString = \"(\" + " + varNames.reduceLeft(_ + " + \",\" + " + _) + "+ \")\"\n"

					if ( accessModifier == "Read" && valueType == IntValueType ) {
						if ( arity == 3 || arity == 2 ) {
							classString += "\tdef to ( end : " + className + " ) = new " + className + ".VecRange(this,end + 1)\n"
							classString += "\tdef until ( end : " + className + " ) = new " + className + ".VecRange(this,end)\n"
						}
					}

					classString += "\tdef resolve = this\n"
					classString += "\tdef baseValue = this\n"

					if ( valueType.floatingPoint ) {
						classString += "\tdef normalize = this / length\n"
						classString += "\tdef normalizeSafe = {\n" +
											"\t\tval l = lengthSafe\n" +
											"\t\tif ( l != 0 ) { this / l } else { new " + writeClassName + "(" + varNames.map(_ => "0").reduceLeft(_ + "," + _) + ") }\n" +
											"\t}\n"
						classString += "\tdef scaleToLength ( l : Float ) = this * ( l / length )\n"

						classString += "\tdef dot ( v : " + className + " ) = (" + varNames.map(n => n + " * v." + n).reduceLeft(_ + "+" + _) + ")\n"
						classString += "\tdef round = new " + this.classNameFor(accessModifier,arity,IntValueType) + "(" + varNames.map(n => n + ".round").reduceLeft(_ + "," + _) + ")\n"

						if ( arity == 3 ) {
							classString += "\tdef cross ( v : " + className + " ) = new " + writeClassName + "(y*v.z-v.y*z,z*v.x-v.z*x,x*v.y-v.x*y)\n"
						}
					}

					classString += "\toverride def equals ( other : Any ) = other match {\n" +
										"\t\tcase v : " + className + " => " + varNames.map(n => n + " == v." + n).reduceLeft(_ + " && " + _) + "\n" +
										"\t\tcase mv : Moddable[" + className + "] => this == mv.resolve()\n" +
										"\t\tcase _ => false" +
										"\n\t}\n"

					if ( arity == 2 ) {
						classString += "\toverride def hashCode = 41 * (41 + y.hashCode) + x.hashCode\n"
					} else if ( arity == 3 ){
						classString += "\toverride def hashCode = 41 * (41 * (41 + z.hashCode) + y.hashCode) + x.hashCode\n"
					} else if ( arity == 4 ) {
						classString += "\toverride def hashCode = 41 * (41 * (41 * (41 + a.hashCode) + b.hashCode) + g.hashCode) + r.hashCode\n"
					}


					classString += "\tdef apply (i:Int) = i match {\n" + varNames.zipWithIndex.map( t => "\t\tcase " + t._2 + " => " + t._1 + "\n").reduceLeft(_ + _) + "\t\tcase _ => " + (if ( valueType.floatingPoint ) { "0.0f" } else { "0" }) + "\n" + "\t}\n"
				} else if ( accessModifier == "" ) {
					for ( varName <- varNames ) {
						classString += "\toverride def " + varName + "_= ( s : " + valueType.name + " ) { " + varName + "i = s }\n"
					}
					for ( (altVarName,varName) <- alternateNamesByArity.getOrElse(arity,Nil).zip(varNames) ) {
						classString += "\toverride def " + altVarName + "_= ( s : " + valueType.name + " ) { " + varName + "i = s }\n"
					}

					for ( op_base <- (operations ::: extraOperationsByType.getOrElse(valueType,Nil)) ; op = op_base + "=" ) {
						classString += "\tdef " + op + "(v : " + classNameFor("Read",arity) + ") { " + varNames.map(n => n + "i " + op + " v." + n + "i").reduceLeft(_ + ";" + _) + "}\n"
						classString += "\tdef " + op + "(s : " + valueType.name + ") {" + varNames.map(n => n + "i " + op + " s").reduceLeft(_ + ";" + _) + "}\n"
					}
					for ( op <- (operations ::: extraOperationsByType.getOrElse(valueType,Nil)) ) {
						classString += "\toverride def " + op + "(v : " + classNameFor("Read",arity) + ") = new " + className + "(" + varNames.map(n => n + "i " + op + " v." + n + "i").reduceLeft(_ + "," + _) + ")\n"
						classString += "\toverride def " + op + "(s : " + valueType.name + ") = new " + className + "(" + varNames.map(n => n + "i " + op + " s").reduceLeft(_ + "," + _) + ")\n"
					}

					classString += "\tdef update (i:Int,s:" + valueType.name + ") { i match {\n" + varNames.zipWithIndex.map( t => "\t\tcase " + t._2 + " => " + t._1 + " = s\n").reduceLeft(_ + _) + "\t\tcase _ => \n" + "\t}}\n"
				}

				classString += "\n}\n"

				classString += "object " + className + "{\n"

				classString += "\tdef apply " + argString + " = new " + className + argString + "\n"
				classString += "\tdef apply (v : " + classNameFor("Read",arity) + ") = new " + className + "(" + varNames.map(n => "v." + n).reduceLeft(_ + "," + _) + ")\n"
				if ( accessModifier == "" ) {
					classString += "\timplicit def toWriteable (v : " + classNameFor("Read",arity) + ") = new " + className + "(" + varNames.map(n => "v." + n).reduceLeft(_ + "," + _) + ")\n"
				}
				if ( accessModifier == "Read" && valueType == IntValueType ) {
					if ( arity == 3 ) {
						classString +=
						"""
						  |	class VecRange(min:ReadVec3i,max:ReadVec3i) extends Traversable[ReadVec3i] {
						  |
						  |  		override def size = (max.x - min.x) * (max.y - min.y) * (max.z - min.z)
						  |		def foreach[U](f: (ReadVec3i) => U) {
						  |			if ( min != max ) {
						  |				val current = Vec3i(min)
						  |				var break = false
						  |				while ( ! break ) {
						  |					f(Vec3i(current))
						  |
						  |					current.x += 1
						  |					if ( current.x >= max.x ) {
						  |						current.x = min.x
						  |						current.y += 1
						  |						if ( current.y >= max.y ) {
						  |							current.y = min.y
						  |							current.z += 1
						  |							if ( current.z >= max.z ) {
						  |								break = true
						  |							}
						  |						}
						  |					}
						  |				}
						  |			}
						  |		}
						  |	}
						  |
						""".stripMargin
					} else if ( arity == 2 ) {
						classString +=
						"""
						  |	class VecRange(min:ReadVec2i,max:ReadVec2i) extends Traversable[ReadVec2i] {
						  |
						  |  		override def size = (max.x - min.x) * (max.y - min.y)
						  |		def foreach[U](f: (ReadVec2i) => U) {
						  |			if ( min != max ) {
						  |   			val current = Vec2i(min)
						  |				var break = false
						  |				while ( ! break ) {
						  |					f(Vec2i(current))
						  |
						  |					current.x += 1
						  |					if ( current.x >= max.x ) {
						  |						current.x = min.x
						  |						current.y += 1
						  |						if ( current.y >= max.y ) {
						  |							break = true
						  |						}
						  |					}
						  |				}
						  |			}
						  |		}
						  |	}
						  |
						""".stripMargin
					}
				}
				if ( ! valueType.floatingPoint ) {
					classString += "\tdef apply (v : " + outer.classNameFor("Read",arity,FloatValueType) + ") = new " + className + "(" + varNames.map(n => "v." + n + ".toInt").reduceLeft(_ + "," + _) + ")\n"
					classString += "\timplicit def toFloatingPoint (v : " + className + ") = new " + outer.classNameFor(accessModifier,arity,FloatValueType) + "(" + varNames.map(n => "v." + n + ".toFloat").reduceLeft(_ + "," + _) + ")\n"
				}
				classString += "\tdef apply (s : " + valueType.name + ") = new " + className + "(" + varNames.map(n => "s").reduceLeft(_ + "," + _) + ")\n"
				if ( arity == 4 && valueType.floatingPoint ) {
					classString += f"\tdef apply (rgb : ${valueType.name}, a : ${valueType.name}) = new $className (s,s,s,a)\n"
				}

				if ( accessModifier == "" ) {
//					classString += "\tval Zero = new " + classNameFor("Read",arity) + "(" + varNames.map(_ => "0").reduceLeft(_ + "," + _) + ")\n"
//					classString += "\tval One = new " + classNameFor("Read",arity) + "(" + varNames.map(_ => "1").reduceLeft(_ + "," + _) + ")\n"

					for ( varName <- varNames ) {
						classString += "\tval Unit" + varName.toUpperCase + " = new " + classNameFor("Read",arity) + "(" + varNames.map(n => if ( n == varName ) { "1" } else { "0" }).reduceLeft(_ + "," + _) + ")\n"
					}

					classString += "\tobject _Identity extends " + classNameFor("Read",arity) + "(" + varNames.map(_ => "1").reduceLeft(_ + "," + _) + ") {\n"
					classString += "\t\toverride def * ( v : " + classNameFor("Read",arity) + " ) = v\n"
					classString += "\t\tdef * ( v : " + className + " ) = v\n"
					classString += "\t}\n"
					classString += "\tval Identity : " + classNameFor("Read",arity) + " = _Identity\n"
					classString += "\tval One : " + classNameFor("Read",arity) + " = _Identity\n"

					classString += "\tobject _Zero extends " + classNameFor("Read",arity) + "{\n"
					classString += "\t\toverride def * ( v : " + classNameFor("Read",arity) + " ) = this\n"
					classString += "\t\tdef * ( v : " + className + " ) = this\n"
					classString += "\t\toverride def / ( v : " + classNameFor("Read",arity) + " ) = this\n"
					classString += "\t\tdef / ( v : " + className + " ) = this\n"
					classString += "\t\toverride def + ( v : " + classNameFor("Read",arity) + " ) = v\n"
					classString += "\t\tdef + ( v : " + className + " ) = v\n"
					classString += "\t}\n"
					classString += "\tval Zero : " + classNameFor("Read",arity) + " = _Zero\n"
				}

				classString += "}\n"
			}


			val fileName = className + (if ( accessModifier == "Intern" ) { ".java" } else { ".scala" })
			Output(fileName,classString)
		}
	}

	def main ( args : Array[String] ) {
		val vs = generate
		for ( v <- vs ) {
			println(v.classContent)
			println("\n\n")

			val outFile = new File("/Users/nvt/Code/anthologicon/src/main/scala/arx/core/vec/" + v.fileName)
//			if ( ! outFile.exists ) {
				val w = new FileWriter(outFile)
				w.write("package arx.core.vec;\n")
				w.write(v.classContent)
				w.close()
//			}
		}
	}

	main(Array(""))
}