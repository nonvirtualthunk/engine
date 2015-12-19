package arx.core.mat

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/19/13
 * Time: 8:56 AM
 * Created by nonvirtualthunk
 */

import java.io.{FileWriter, File}
import arx.core.vec.Vec2i

object MatrixGenerator {
	case class Output ( fileName : String , classContent : String )
	case class ValueType ( name : String , javaName : String , char : String , defaultValue : String , floatingPoint : Boolean )
	val FloatValueType = ValueType("Float","float","f","0.0f",floatingPoint = true)

	val AccessModifiers = List("Read","")
	val Arities = for ( i <- 2 to 4 ; j <- 2 to 4 ) yield { Vec2i(i,j) }

	val scalarOperations = List("+","-","*","/")
	val piecewiseMatrixOperations = List("+","-")

	def classNameFor(mod:String,ar:Vec2i) = mod + "Mat" + ar.x + "x" + ar.y
	val outer = this

	def varNamesByArity ( arity : Vec2i ) = for( y <- 0 until arity.y; x <- 0 until arity.x ) yield { varNameFor(x,y) }
	def varNameFor( x : Int , y : Int ) = "m" + x + y

	def generate = {
		val valueType = FloatValueType
		for ( accessModifier <- AccessModifiers ; arity <- Arities ) yield {
			var classString = ""
			def classNameForAccessModifier(m:String) = classNameFor(m,arity)
			val className = classNameForAccessModifier(accessModifier)
			val varNames = varNamesByArity(arity)
			val varNamesi = varNames.map(_ + "i")

			val argString = "(" + varNames.map(n => n + "a : " + valueType.name).reduceLeft(_ + "," + _) + ")"

			classString += "import arx.core.vec._\n"
			if ( accessModifier == "Read" ) {
				classString += "import arx.core.Moddable\n"
				classString += "import arx.core.SelfModdable\n"
				classString += "import java.io.Externalizable\n"
				classString += "import java.io.ObjectInput\n"
				classString += "import java.io.ObjectOutput\n"
				classString += "\n"
			}

			classString += "@SerialVersionUID(9223372036854770000L)\n"
			classString += "class " + className
			if ( accessModifier == "Read" ) { classString += " extends " + "Externalizable" + " with SelfModdable[" + className + "]" }
			else if ( accessModifier == "" ) { classString += " extends " + classNameForAccessModifier("Read") }
			classString += "{\n"

			classString += "\tdef this" + argString + "{ \n\t\tthis()\n" + varNames.map(n => "\t\t" + n + "i = " + n + "a" + "\n").reduceLeft(_ + _) + "\t}\n"

			if ( accessModifier == "Read" ) {

				varNamesi.foreach( n => classString += "protected var " + n + " : " + valueType.name + " = " + valueType.defaultValue + "\n" )

				for ( op <- piecewiseMatrixOperations ) {
					classString += "\tdef " + op + "(m : Moddable[" + className + "]) = { val v = m.resolve(); new " + className + "(" + varNames.map(n => n + "i " + op + " v." + n + "i").reduceLeft(_ + "," + _) + ") }\n"
					classString += "\tdef " + op + "(v : " + className + ") = new " + className + "(" + varNames.map(n => n + "i " + op + " v." + n + "i").reduceLeft(_ + "," + _) + ")\n"
				}
				for ( op <- scalarOperations ) {
					classString += "\tdef " + op + "(s : " + valueType.name + ") = new " + className + "(" + varNames.map(n => n + "i " + op + " s").reduceLeft(_ + "," + _) + ")\n"
				}

				for ( p <- 1 to 4 ) {
					val m = arity.x ; val n = arity.y
					val otherArity = Vec2i(n,p)
					val (otherClassName,retClassName) = if ( p == 1 ) {
						(if ( n == 2 ) { "ReadVec2f" }
						else if ( n == 3 ) { "ReadVec3f" }
						else if ( n == 4 ) { "ReadVec4f" }
						else { "WAT" -> "WAT" }) -> (
						if (m == 2 ) { "ReadVec2f" }
						else if ( m == 3 ) { "ReadVec3f" }
						else if ( m == 4 ) { "ReadVec4f" }
						)
					} else { (classNameFor(accessModifier,otherArity),classNameFor(accessModifier,Vec2i(m,p))) }

					classString += "\tdef * (other : " + otherClassName + ") : " + retClassName + " = new " + retClassName + "(\n" +
											(for ( j <- 0 until p ; i <- 0 until m ) yield {
												(for ( k <- 0 until n ) yield {
													if ( p != 1 ) {
														varNameFor(i,k) + " * other." + varNameFor(k,j)
													} else {
														varNameFor(i,k) + " * other." + (
														if ( n != 4 ) {
															if ( k == 0 ) { "x" }
															else if ( k == 1 ) { "y" }
															else { "z" }
														} else {
															k match {
																case 0 => "r"
																case 1 => "g"
																case 2 => "b"
																case 3 => "a"
																case _ => "WAT"
															}
														})
													}
												}).reduceLeft(_ + " + " + _)
										  }).reduceLeft(_ + ",\n" + _) + "\n)\n"
				}

				if ( arity == Vec2i(3,4) ) {
					classString += "\tdef transformPoint ( p : ReadVec3f ) = ReadVec3f(m00*p.x + m01*p.y + m02*p.z + m03,m10*p.x + m11*p.y + m12*p.z + m13,m20*p.x + m21*p.y + m22*p.z + m23)\n"
					classString += "\tdef transformVector ( p : ReadVec3f ) = ReadVec3f(m00*p.x + m01*p.y + m02*p.z,m10*p.x + m11*p.y + m12*p.z,m20*p.x + m21*p.y + m22*p.z)\n"
					
					classString += "\tdef rotateX ( theta : Float ) = {" +
										"\t\tval sin = math.sin(theta).toFloat; val cos = math.cos(theta).toFloat\n" +
										"\t\tnew ReadMat3x4(\n" +
										"\t\tm00, cos*m10 - sin*m20, sin*m10 + cos*m20,\n" +
										"\t\tm01, cos*m11 - sin*m21, sin*m11 + cos*m21,\n" +
										"\t\tm02, cos*m12 - sin*m22, sin*m12 + cos*m22,\n" +
										"\t\tm03, cos*m13 - sin*m23, sin*m13 + cos*m23\n" +
										")}\n"
					classString += "\tdef rotateY ( theta : Float ) = {" +
										"\t\tval sin = math.sin(theta).toFloat; val cos = math.cos(theta).toFloat\n" +
										"\t\tnew ReadMat3x4(\n" +
										"\t\tcos*m00 + sin*m20, m10, cos*m20 - sin*m00,\n" +
										"\t\tcos*m01 + sin*m21, m11, cos*m21 - sin*m01,\n" +
										"\t\tcos*m02 + sin*m22, m12, cos*m22 - sin*m02,\n" +
										"\t\tcos*m03 + sin*m23, m13, cos*m23 - sin*m03\n" +
										")}\n"
					classString += "\tdef rotateZ ( theta : Float ) = {" +
										"\t\tval sin = math.sin(theta).toFloat; val cos = math.cos(theta).toFloat\n" +
										"\t\tnew ReadMat3x4(\n" +
										"\t\tcos*m00 - sin*m10, sin*m00 + cos*m10, m20,\n" +
										"\t\tcos*m01 - sin*m11, sin*m01 + cos*m11, m21,\n" +
										"\t\tcos*m02 - sin*m12, sin*m02 + cos*m12, m22,\n" +
										"\t\tcos*m03 - sin*m13, sin*m03 + cos*m13, m23\n" +
										")}\n"
				}

				classString += "\n\tdef readExternal ( in : ObjectInput ) {\n" + varNamesi.map( n => n + " = in.read" + valueType.name).reduceLeft(_ + "\n" + _) + "\n}\n"
				classString += "\n\tdef writeExternal ( out : ObjectOutput ) {\n" + varNamesi.map( n => "out.write" + valueType.name + "(" + n + ")").reduceLeft(_ + "\n" + _) + "\n}\n"

				for ( varName <- varNames ) {
					classString += "\tdef " + varName + " = " + varName + "i\n"
					//TODO: add plusX, minusX, plusY, etc.
//						classString += "\tdef plus" + varName.capitalize + " ( f : " +
					classString += "\tprotected def " + varName + "_= ( s : " + valueType.name + " ) { " + varName + "i = s }\n"
				}
				classString += "\toverride def toString = \"(\" + " + varNames.reduceLeft(_ + " + \",\" + " + _) + "+ \")\"\n"

				classString += "\tdef resolve = this\n"
				classString += "\tdef baseValue = this\n"


				classString += "\toverride def equals ( other : Any ) = other match {\n" +
									"\t\tcase v : " + className + " => " + varNames.map(n => n + " == v." + n).reduceLeft(_ + " && " + _) + "\n" +
									"\t\tcase mv : Moddable[" + className + "] => this == mv.resolve()\n" +
									"\t\tcase _ => false" +
									"\n\t}\n"

				classString += "\toverride def hashCode = " + varNames.tail.foldLeft("41 + m00.hashCode"){ case (str,n) => "41 * (" + str + ") + " + n + ".hashCode" }

				//classString += "\tdef apply (i:Int) = i match {\n" + varNames.zipWithIndex.map( t => "\t\tcase " + t._2 + " => " + t._1 + "\n").reduceLeft(_ + _) + "\t\tcase _ => " + (if ( valueType.floatingPoint ) { "0.0f" } else { "0" }) + "\n" + "\t}\n"
			} else if ( accessModifier == "" ) {
				for ( varName <- varNames ) {
					classString += "\toverride def " + varName + "_= ( s : " + valueType.name + " ) { " + varName + "i = s }\n"
				}

				for ( op_base <- piecewiseMatrixOperations ; op = op_base + "=" ) {
					classString += "\tdef " + op + "(v : " + classNameFor("Read",arity) + ") { " + varNames.map(n => n + "i " + op + " v." + n).reduceLeft(_ + ";" + _) + "}\n"
				}
				for ( op <- (piecewiseMatrixOperations) ) {
					classString += "\toverride def " + op + "(v : " + classNameFor("Read",arity) + ") = new " + className + "(" + varNames.map(n => n + "i " + op + " v." + n).reduceLeft(_ + "," + _) + ")\n"
				}
				for ( op_base <- scalarOperations ; op = op_base + "=" ) {
					classString += "\tdef " + op + "(s : " + valueType.name + ") {" + varNames.map(n => n + "i " + op + " s").reduceLeft(_ + ";" + _) + "}\n"
				}
				for ( op <- (scalarOperations) ) {
					classString += "\toverride def " + op + "(s : " + valueType.name + ") = new " + className + "(" + varNames.map(n => n + "i " + op + " s").reduceLeft(_ + "," + _) + ")\n"
				}

//					classString += "\tdef update (i:Int,s:" + valueType.name + ") { i match {\n" + varNames.zipWithIndex.map( t => "\t\tcase " + t._2 + " => " + t._1 + " = s\n").reduceLeft(_ + _) + "\t\tcase _ => \n" + "\t}}\n"
			}

			classString += "\n}\n"

			classString += "object " + className + "{\n"

			classString += "\tdef apply " + argString + " = new " + className + argString + "\n"
			classString += "\tdef apply (v : " + classNameFor("Read",arity) + ") = new " + className + "(" + varNames.map(n => "v." + n).reduceLeft(_ + "," + _) + ")\n"
			if ( accessModifier == "" ) {
				classString += "\timplicit def toWriteable (v : " + classNameFor("Read",arity) + ") = new " + className + "(" + varNames.map(n => "v." + n).reduceLeft(_ + "," + _) + ")\n"
			}

			if ( arity == Vec2i(4,4) ) {
				classString += s"\tdef apply (s : ${valueType.name}) = new $className(s,0,0,0,0,s,0,0,0,0,s,0,0,0,0,s)\n"
			}

			if ( accessModifier == "" && arity.x == arity.y ) {
//					classString += "\tval Zero = new " + classNameFor("Read",arity) + "(" + varNames.map(_ => "0").reduceLeft(_ + "," + _) + ")\n"
//					classString += "\tval One = new " + classNameFor("Read",arity) + "(" + varNames.map(_ => "1").reduceLeft(_ + "," + _) + ")\n"

				classString += "\tobject _Identity extends " + classNameFor("Read",arity) + "(" + varNames.map(n => if ( n(1) == n(2) ) { "1" } else { "0" }).reduceLeft(_ + "," + _) + ") {\n"
				classString += "\t\toverride def * ( v : " + classNameFor("Read",arity) + " ) = v\n"
				classString += "\t\tdef * ( v : " + className + " ) = v\n"
				classString += "\t}\n"
				classString += "\tval Identity : " + classNameFor("Read",arity) + " = _Identity\n"
			}
			if ( accessModifier == "" && arity == Vec2i(3,4) ) {
				classString += "\tdef rotateX ( theta : Float ) = {" +
									"\t\tval sin = math.sin(theta).toFloat; val cos = math.cos(theta).toFloat\n" +
									"\t\tnew ReadMat3x4(\n" +
									"\t\t1, 0, 0,\n" +
									"\t\t0, cos, sin,\n" +
									"\t\t0, -sin, cos,\n" +
									"\t\t0, 0, 0\n" +
									")}\n"
				classString += "\tdef rotateY ( theta : Float ) = {" +
									"\t\tval sin = math.sin(theta).toFloat; val cos = math.cos(theta).toFloat\n" +
									"\t\tnew ReadMat3x4(\n" +
									"\t\tcos, 0, -sin,\n" +
									"\t\t0, 1, 0,\n" +
									"\t\tsin, 0, cos,\n" +
									"\t\t0, 0, 0\n" +
									")}\n"
				classString += "\tdef rotateZ ( theta : Float ) = {" +
									"\t\tval sin = math.sin(theta).toFloat; val cos = math.cos(theta).toFloat\n" +
									"\t\tnew ReadMat3x4(\n" +
									"\t\tcos, sin, 0,\n" +
									"\t\t-sin, cos, 0,\n" +
									"\t\t0, 0, 1,\n" +
									"\t\t0, 0, 0\n" +
									")}\n"
			} else if ( accessModifier == "" && arity == Vec2i(3,3) ) {
				classString += "\tdef apply ( V0 : ReadVec3f , V1 : ReadVec3f , V2 : ReadVec3f ) = {\n" +
									"\t\tnew ReadMat3x3( V0.x,V0.y,V0.z, V1.x,V1.y,V1.z,V2.x,V2.y,V2.z)\n" +
									"\t}\n"

//				classString += "\tdef inverse ( m : Mat3x3 ) = {\n" +
//									"\t\tval c0 = m.m11*m.m22 - m.m12*m.m21\n" +
//									"\t\tval c1 = m.m12*m.m20 - m.m10*m.m22\n" +
//									"\t\tval c2 = m.m10*m.m21 - m.m11*m.m20\n" +
//									"\t\tval determinant = m.m00*c0 + m.m01*c1 + m.m02*c2\n" +
//									"\t\tif ( determinant != 0.0f ) {\n" +
//									"\t\t\tval inv = 1.0f / determinant\n" +
//									"\t\t\tnew Matx3x(c0*inv,c1*inv,c2*inv," +
//									"\t\t\t\t(m.m02*m.m21 - " +
//									"\t\t} else {\n" +
//									"\t\t\tIdentity" +
//									"\t\t}\n"
			}
			classString += "}\n"


			val fileName = className + ".scala"
			Output(fileName,classString)
		}
	}

	def main ( args : Array[String] ) {
		val vs = generate
		for ( v <- vs ) {
			println(v.classContent)
			println("\n\n")

			val outFile = new File("/Users/nvt/Code/anthologicon/src/main/scala/arx/core/mat/" + v.fileName)
//			if ( ! outFile.exists ) {
				val w = new FileWriter(outFile)
				w.write("package arx.core.mat;\n")
				w.write(v.classContent)
				w.close()
//			}
		}
	}
}