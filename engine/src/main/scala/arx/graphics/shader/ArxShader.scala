package arx.graphics.shader

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/9/13
 * Time: 8:17 PM
 * Created by nonvirtualthunk
 */

import arx.application.Application
import arx.application.Noto
import arx.core.Moddable
import arx.core.introspection.ReflectionAssistant
import arx.core.mat.ReadMat4x4
import arx.core.vec._
import arx.graphics.GL
import org.lwjgl.opengl._

import scala.collection.mutable

abstract class ArxShader extends TShader {
	import ArxShader._

	var version = 150

	var attributes : List[Attribute[_]] = Nil
	var varyings : List[Varying[_]] = Nil
	var uniforms : List[Uniform[_]] = Nil

	def vertexShaderBody : String
	def fragmentShaderBody : String

	var vertexFunctionBodies : List[String] = Nil
	var fragmentFunctionBodies : List[String] = Nil

	protected def effectiveManifestForName ( man : Manifest[_] ) = if ( man.runtimeClass == classOf[Vector[_]] ) { man.typeArguments.head } else { man }
	protected def toGLSLVarTypeName ( v : TVarType ) = {
		val man = effectiveManifestForName(v.vartype)

		if ( man == manifest[Float] ) { "float" }
		else if ( man == manifest[Sampler] ) { "sampler2D" }
		else if ( man == manifest[Sampler3D] ) { "sampler3D" }
		else if ( man == manifest[Int] ) { "int" }
		else if ( man == manifest[Boolean] ) { "bool" }
		else if ( man == manifest[Vec2f] ) { "vec2" }
		else if ( man == manifest[Vec3f] ) { "vec3" }
		else if ( man == manifest[Vec4f] ) { "vec4" }
		else if ( man == manifest[ReadVec2f] ) { "vec2" }
		else if ( man == manifest[ReadVec3f] ) { "vec3" }
		else if ( man == manifest[ReadVec4f] ) { "vec4" }
		else if ( man == manifest[ReadVec4i] ) { "vec4" }
		else if ( man == manifest[Vec4i] ) { "vec4" }
		else if ( man == manifest[ReadMat4x4] ) { "mat4" }
		else {
			Noto.warn("Unknown type in variable : " + man); "void" }
	}

	protected def toAritySuffix (arity : Int) = arity match {
		case 1 => ""
		case n => f"[$n]"
	}


	protected def attribute[T : Manifest] = { val add = new Attribute[T] ; attributes :+= add; add }
	protected def uniform[T : Manifest](initialValue : T) : Uniform[T] = {
		if (manifest[T].runtimeClass == classOf[Vector[_]]) { uniform(initialValue,initialValue.asInstanceOf[Vector[_]].size) }
		else { val add = new Uniform[T](initialValue,this); uniforms :+= add; add }
	}
	protected def uniform[T : Manifest](initialValue : T, arity : Int) : Uniform[T] = { val add = new Uniform[T](initialValue,this);add.arity = arity;uniforms :+= add; add }
	protected def varying[T : Manifest] = { val add = new Varying[T]; varyings :+= add; add }

	protected val varDecl = Set("float","int","bool","vec2","vec3","vec4")

	protected def processBody ( str : String ) = {
		val retBuilder = new StringBuilder
		val lines = str.split('\n')
		var include = true
		for ( line <- lines ) {
			val tokens = line.trim.split(' ')

			if ( tokens.length > 0 ) {
				if ( tokens(0) == "#if" || tokens(0) == "#elseif" ) {
					if ( tokens(0) != "#elseif" || ! include ) {
						val predicate = tokens(1)
						if ( predicate == "active" ) {
							val varName = tokens(2)
							include = glslVariablesByName.get(varName) match {
								case Some(v) if ( v.active.resolve() ) => true
								case _ => false
							}
						} else { // custom predicate
							include = this.getClass.getMethod(predicate).invoke(this).asInstanceOf[Boolean]
						}
					} else { include = false }
				} else if ( tokens(0) == "#else" ) {
					include = ! include
				} else if ( tokens(0) == "#end" ) {
					include = true
				} else if ( include ) {
					var i = 0; while ( i < tokens.length ) {
						if ( varDecl(tokens(i)) ) {
							val next = tokens(i+1)
							glslVariablesByName.get(next) match {
								case Some(v) if v.active.resolve() => tokens(i) = "" //erase the var
								case _ => //actual local variable, carry on
							}
						}
						i += 1
					}
					tokens.foreach(tok => { retBuilder.append(tok); retBuilder.append(" ") } )
					retBuilder.append("\n")
				}
			}
		}
		retBuilder.toString
	}

	val uniformLocations = mutable.HashMap[String, Int]()
	protected val self = this
	protected lazy val allFields = ReflectionAssistant.collectAllFieldsFrom(this.getClass)
	protected lazy val vartypeFields = allFields.filter(f => classOf[TVarType].isAssignableFrom(f.getType)).toList
	protected lazy val glslNamesAndVariables = vartypeFields.map { f =>
		val variable = self.getClass.getMethod(f.getName).invoke(self).asInstanceOf[TVarType]
		variable.name = f.getName
		f.getName -> variable
	}
	protected lazy val glslVariablesByName = glslNamesAndVariables.toMap

	def createShaderTexts = {
		glslVariablesByName

		var vertexRet = ""
		var fragmentRet = ""

		def appendBoth (str:String) { vertexRet += str;fragmentRet += str }

		appendBoth("#version " + version + " core\n")

		def strFor ( value : TVarType, isVertexShader : Boolean ) = {
			f"${value.symbol(isVertexShader)} ${toGLSLVarTypeName(value)} ${value.name}${toAritySuffix(value.arity)};"
		}
		for ( value <- attributes if value.active.resolve() ) { vertexRet += strFor(value,isVertexShader = true) }
		vertexRet += "\n"
		for ( value <- uniforms if value.active.resolve() ) { appendBoth(strFor(value,isVertexShader = true)) }
		appendBoth("\n")
		for ( value <- varyings if value.active.resolve() ) { vertexRet += (strFor(value,isVertexShader = true)) }
		for ( value <- varyings if value.active.resolve() ) { fragmentRet += (strFor(value,isVertexShader = false)) }
		appendBoth("\n")

		for ( func <- vertexFunctionBodies ) { vertexRet += func + "\n" }
		for ( func <- fragmentFunctionBodies ) { fragmentRet += func + "\n" }

		vertexRet += "\n"
		vertexRet += processBody(vertexShaderBody)

		fragmentRet += "\n"
		fragmentRet += processBody(fragmentShaderBody)

		Noto.finest("Vertex Shader text : \n" + vertexRet + "\n\nFragment Shader text : \n" + fragmentRet + "\n####################################")

		(vertexRet,fragmentRet)
	}

	def addVertexFunction (str:String){ vertexFunctionBodies ::= str }
	def addFragmentFunction (str:String){ fragmentFunctionBodies ::= str }

	def setUniform(uniformName: String, v: ReadMat4x4, tolerateAbsence: Boolean ) {
		val location = uniformLocations.getOrElseUpdate(uniformName, GL20.glGetUniformLocation(shaderObject, uniformName))
		if (location != -1) {
			val buf = GL.toTmpBuffer(v)
			GL.backing.glUniformMatrix4(location,false,buf)
		} else if (!tolerateAbsence) {
			throw new IllegalStateException("Attempting to set invalid uniform \"" + uniformName + "\" on shader \"" + this.getClass.getSimpleName + "\"")
		}
	}

	//+=============================+ OPENGL CODE +=============================+

	var vertexShaderId = 0
	var fragmentShaderId = 0
	var shaderObject = 0
	var dirty = false
	var autoUpdate = false
	var fragmentShaderHash = 0
	var vertexShaderHash = 0

	def bind () {
		if ( !dirty && autoUpdate && (fragmentShaderHash != fragmentShaderBody.hashCode || vertexShaderHash != vertexShaderBody.hashCode)) {
			Noto.info(s"Re-committing altered shader: ${this.getClass.getSimpleName}")
			dirty = true
		}
		if ( shaderObject == 0 || dirty ) {
			commitShader()
		}
		Shader.boundShader = Some(this)
		GL.bindShader(shaderObject)
		uniforms.foreach( _.commit()  )
	}
	def isBound = GL.activeShader == shaderObject

	def commitShader () {
		uniforms.foreach( _.reset() )

		val (vert,frag) = createShaderTexts
		vertexShaderHash = vertexShaderBody.hashCode
		fragmentShaderHash = fragmentShaderBody.hashCode

		if ( GL.checkError() ) { Noto.warn("\terror encountered before shader compilation") }

		vertexShaderId = vertexShaderId match { case 0 => GL20.glCreateShader(GL20.GL_VERTEX_SHADER) ; case id => id }
		GL20.glShaderSource(vertexShaderId,vert)
		GL20.glCompileShader(vertexShaderId)
		if ( ! getInfo(vertexShaderId,compilation = true) ) {
			Noto.info("Full Vertex Shader Text : ")
			Noto.info("\t" + vert.replace("\n","\n\t"))
		}
		if ( GL.checkError() ) {
			Noto.warn("\terror encountered vertex shader compile") }

		fragmentShaderId = fragmentShaderId match { case 0 => GL20.glCreateShader(GL20.GL_FRAGMENT_SHADER) ; case id => id }
		GL20.glShaderSource(fragmentShaderId,frag)
		GL20.glCompileShader(fragmentShaderId)
		if ( ! getInfo(fragmentShaderId,compilation = true) ) {
			Noto.info("Full Fragment Shader Text : ")
			Noto.info("\t" + frag.replace("\n","\n\t"))
		}
		if ( GL.checkError() ) { Noto.warn("\terror encountered fragment shader compile") }

		shaderObject = shaderObject match {
			case 0 => GL20.glCreateProgram()
			case id => id
		}

		GL20.glAttachShader(shaderObject, vertexShaderId)
		GL20.glAttachShader(shaderObject, fragmentShaderId)

		if ( GL.checkError() ) { Noto.warn("\terror encountered shader object attach") }


		for ( (variable,i) <- attributes.filter(_.active.resolve()).zipWithIndex ) {
			GL20.glBindAttribLocation(shaderObject,i,variable.name)
		}
		if ( GL.checkError() ) { Noto.warn("\terror encountered shader attrib bind") }

		GL20.glLinkProgram(shaderObject)
		GL20.glValidateProgram(shaderObject)
		getInfo(shaderObject,compilation = false)

		if ( GL.checkError() ) { Noto.warn("\terror encountered in shader commit") }

		dirty = false
	}

	def getInfo(id: Int, compilation : Boolean): Boolean = {
		val success = if ( compilation ) {
			GL20.glGetShaderi(id,GL20.GL_COMPILE_STATUS) == GL11.GL_TRUE
		} else {
			GL20.glGetProgrami(id,GL20.GL_LINK_STATUS) == GL11.GL_TRUE
		}

		if (! success) {
			val log = if ( compilation ) { GL20.glGetShaderInfoLog(id,1000) }
			else { GL20.glGetProgramInfoLog(id,1000) }
			println(s"Info log: $log")
			false
		} else {
			true
		}
	}


	ArxShader.allShaders ::= this
}

object ArxShader {
	case class Sampler(i:Int)
	case class Sampler3D(i : Int)
	implicit def int2sampler ( i : Int ): Sampler = new Sampler(i)
	implicit def int2sampler3d ( i : Int ): Sampler3D = new Sampler3D(i)

	trait TVarType {
		def symbol (isVertexShader:Boolean): String
		def vartype : Manifest[_]
		var active = Moddable(true)
		var name = ""
		var arity = 1
	}
	class Varying[T : Manifest] extends TVarType{
		def symbol(isVertexShader:Boolean): String = {
			val interpolater = if (manifest[T] == manifest[Int]) { "flat " } else {""}
			val base = if ( isVertexShader ) { "out" } else { "in" }
			interpolater + base
		}
		def vartype = manifest[T]
	}
	class Attribute[T : Manifest] extends TVarType{
		def symbol(isVertexShader:Boolean) = if ( isVertexShader ) { "in" } else { "no attributes in frag shader" }
		def vartype = manifest[T]
	}
	class Uniform[T : Manifest](defaultValue : T,inShader : ArxShader) extends TVarType{
		var location = 0
		var valueInShader : Option[T] = None
		var desiredValueStack : List[Moddable[T]] = List(Moddable(defaultValue))

		def reset (): Unit = {
			location = 0
			valueInShader = None
		}

		def commit () {
			if ( active.resolve() && inShader.shaderObject > 0 && inShader.isBound && Application.isOpenGLThread ) {
				if ( GL.debugMode ) { GL.checkError() }

				val curHead : T = desiredValueStack.head.resolve()
				if ( valueInShader.isEmpty || curHead != valueInShader.get ) {
					if ( location == 0 ) { location = GL20.glGetUniformLocation(inShader.shaderObject,name) }

					curHead match {
						case f : Float => GL20.glUniform1f(location,f)
						case i : Int => GL20.glUniform1i(location,i)
						case s : Sampler => GL20.glUniform1i(location,s.i)
						case s3 : Sampler3D => GL20.glUniform1i(location,s3.i)
						case v2 : ReadVec2f => GL20.glUniform2f(location,v2.x,v2.y)
						case v3 : ReadVec3f => GL20.glUniform3f(location,v3.x,v3.y,v3.z)
						case v4 : ReadVec4f => GL20.glUniform4f(location,v4.r,v4.g,v4.b,v4.a)
						case m4 : ReadMat4x4 => GL.backing.glUniformMatrix4(location,false,GL.toTmpBuffer(m4))
						case vec : Vector[_] => {
							if ( manifest[T].typeArguments.head == manifest[ReadVec3f] ) {
								GL20.glUniform3fv(location,GL.toTmpBuffer3f(vec.asInstanceOf[Vector[ReadVec3f]]))
							} else if ( manifest[T].typeArguments.head == manifest[Float] ) {
								GL20.glUniform1fv(location,GL.toTmpBufferf(vec.asInstanceOf[Vector[Float]]))
							} else {
								throw new IllegalStateException("WAT 3f")
							}
						}
						case _ => Noto.warn("Invalid type in desired value of uniform : " + name + " loc : " + location + " value : " + desiredValueStack.head)
					}

					valueInShader = Some(curHead)
				}

				if ( GL.debugMode ) { GL.checkError() }
			}
		}

		def set ( t : Moddable[T] ) {
			desiredValueStack = desiredValueStack.updated(0,t)
			commit()
		}
		def push ( t : Moddable[T] ) {
			desiredValueStack ::= t
			commit()
		}
		def pop () {
			desiredValueStack = desiredValueStack.tail
			commit()
		}

		def symbol ( unused : Boolean ) = "uniform"
		def vartype = manifest[T]
	}

	case class VarType ( clazz : Class[_] , str : String )

	var allShaders : List[ArxShader] = Nil
}