package arx.graphics.shader

import java.io.InputStream

import arx.application.Application
import arx.application.Noto
import arx.core.mat.ReadMat4x4
import arx.core.vec.{ReadVec3f, ReadVec4f, Vec2f, Vec3f, Vec4f}
import arx.graphics.GL
import org.lwjgl.opengl._

import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.mutable.SynchronizedQueue

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 9/26/11
 * Time: 9:09 PM
 * Created by nonvirtualthunk
 */

class Shader extends TShader {
	var vertexShader: Int = 0
	var fragmentShader: Int = 0
	var shaderObject: Int = 0
	val uniformLocations = HashMap[String, Int]()
	var onBind: (Shader) => Unit = (s: Shader) => {}
	var deferred = new SynchronizedQueue[() => Unit]
	var onNextBind = new SynchronizedQueue[() => Unit]
	var name = "Default Shader Name"

	val attributeMap : HashMap[String,Int] = new HashMap[String,Int]()
	val uniformStacks = new HashMap[String,Stack[Any]]()

	def bind() {
		if ( shaderObject == 0 || ! isBound ) {
			while ( deferred.nonEmpty ) {
				val dfunc = deferred.dequeue()
				dfunc()
			}

			Shader.boundShader = Some(this)
			GL.bindShader(shaderObject)

			while ( onNextBind.nonEmpty ) {
				(onNextBind.dequeue())()
			}
			onBind(this)
		}
	}

	def isBound = GL.activeShader == shaderObject

	def doWhileBound ( stmt : => Unit ) {
		if ( ! GL.disabled ) {
			if ( isBound && Application.isOpenGLThread ) {
				stmt
			} else {
				onNextBind.enqueue( () => stmt )
			}
		}
	}

	def loadFromStreams(vertexStream : InputStream, fragmentStream: InputStream): Shader = {
		if (!Application.isOpenGLThread) {
			deferred.enqueue( () => loadFromStreams(vertexStream,fragmentStream) )
		}
		else {
			uniformLocations.clear()
			attributeMap.clear()

			if (vertexStream != null) {
				loadShader(vertexStream, isVertexShader = true)
			}
			if (fragmentStream != null) {
				loadShader(fragmentStream, isVertexShader = false)
			}
			if ( vertexStream == null && fragmentStream == null ) {
				Noto.severeError("Totally invalid shader loading attempt")
			}

			//			if ( shaderObject == 0 ) {
			shaderObject = GL20.glCreateProgram()
			//			}

			GL20.glAttachShader(shaderObject, vertexShader)
			GL20.glAttachShader(shaderObject, fragmentShader)

			for ( (name,index) <- attributeMap ) {
				bindAttribLocation(index,name)
			}

			GL20.glLinkProgram(shaderObject)
			GL20.glValidateProgram(shaderObject)
			getInfo(shaderObject,compilation = false)
		}

		this
	}
	/** This will load the shader and then close the stream, since it may deffer the actual
	  * loading, do not close the stream yourself
	  *
	  * @param stream
	  * @param isVertexShader
	  */
	def loadShader(stream: InputStream, isVertexShader: Boolean) {
		if (!Application.isOpenGLThread) {
			deferred.enqueue( () => loadShader(stream, isVertexShader) )
		}
		else {
			val shader =
				if (isVertexShader) {
					//					if ( vertexShader == 0 ) {
					vertexShader = GL20.glCreateShader(GL20.GL_VERTEX_SHADER)
					//					}
					vertexShader
				}
				else {
					//					if ( fragmentShader == 0 ) {
					fragmentShader = GL20.glCreateShader(GL20.GL_FRAGMENT_SHADER)
					//					}
					fragmentShader
				}


			val source = scala.io.Source.fromInputStream(stream)

			val shaderSource = new StringBuffer
			var i = 0
			for ( line <- source.getLines() ) {
				val tline = line.trim
				if ( tline.startsWith("in") && isVertexShader ){
					//drop the semicolon, split on whitespace
					val sections = tline.takeWhile(char => char != '/' && char != ';').trim.split(" ")
					attributeMap(sections(2)) = i
					i += 1
				}
				shaderSource.append(tline).append("\n")
			}

			source.close()
			stream.close()

			GL20.glShaderSource(shader, shaderSource)
			GL20.glCompileShader(shader)

			if (!getInfo(shader,true)) {
				println("ERROR, following source failed to compile :")
				println(shaderSource)
			}

		}
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
		}
		true
	}



	def pushUniform(uniformName: String, f: Float ) {
		if ( ! Application.isOpenGLThread || ! isBound ) { throw new IllegalStateException("Pushing a uniform makes no sense if not on the opengl thread, or not bound") }
		val stack = uniformStacks.getOrElseUpdate(uniformName,{ val s = new Stack[Any]();s.push(f);s})
		stack.push(f)


		val location = uniformLocations.getOrElseUpdate(uniformName, GL20.glGetUniformLocation(shaderObject, uniformName))
		if (location != -1) {
			GL20.glUniform1f(location, f)
		}

	}
	def popUniform(uniformName: String ) {
		if ( ! Application.isOpenGLThread || ! isBound ) { throw new IllegalStateException("Popping a uniform makes no sense if not on the opengl thread, or not bound") }
		val stack = uniformStacks.getOrElse(uniformName,new Stack())
		stack.pop()
		stack.top match {
			case f : Float => setUniform(uniformName,f)
			case i : Int => setUniform(uniformName,i)
			case v : Vec2f => setUniform(uniformName,v)
			case v : Vec3f => setUniform(uniformName,v)
			case v : Vec4f => setUniform(uniformName,v)
			case _ => Noto.error("Illegal type with pop uniform")
		}
	}
	def setUniform(uniformName: String, f: Float ) { setUniform(uniformName,f,false) }
	def setUniform(uniformName: String, f: Float, tolerateAbsence: Boolean ) {
		doWhileBound {
			val stack = uniformStacks.getOrElseUpdate(uniformName,new Stack())
			if ( stack.isEmpty || stack.top != f ) {
				if ( stack.nonEmpty ) { stack.pop() }
				stack.push(f)

				val location = uniformLocations.getOrElseUpdate(uniformName, GL20.glGetUniformLocation(shaderObject, uniformName))
				if (location != -1) {
					GL20.glUniform1f(location, f)
				} else if (!tolerateAbsence) {
					throw new IllegalStateException("Attempting to set invalid uniform \"" + uniformName + "\" on shader \"" + name + "\"")
				}
			}
		}
	}

	def setUniform(uniformName: String, i: Int) { setUniform(uniformName,i,false) }
	def setUniform(uniformName: String, i: Int, tolerateAbsence: Boolean) {
		doWhileBound {
			val stack = uniformStacks.getOrElseUpdate(uniformName,new Stack())
			if ( stack.isEmpty || stack.top != i ) {
				if ( stack.nonEmpty ) { stack.pop() }
				stack.push(i)

				val location = uniformLocations.getOrElseUpdate(uniformName, GL20.glGetUniformLocation(shaderObject, uniformName))
				if (location != -1) {
					GL20.glUniform1i(location, i)
				} else if (!tolerateAbsence) {
					throw new IllegalStateException("Attempting to set invalid uniform \"" + uniformName + "\" on shader \"" + name + "\"")
				}
			}
		}
	}

	def setUniform(uniformName: String, v: Vec2f ) { setUniform(uniformName,v,false); }
	def setUniform(uniformName: String, v: Vec2f, tolerateAbsence: Boolean) {
		doWhileBound {
			val stack = uniformStacks.getOrElseUpdate(uniformName,new Stack())
			if ( stack.isEmpty || ( stack.top != v ) ) {
				if ( stack.nonEmpty ) { stack.pop() }
				stack.push(v)

				val location = uniformLocations.getOrElseUpdate(uniformName, GL20.glGetUniformLocation(shaderObject, uniformName))
				if (location != -1) {
					GL20.glUniform2f(location, v.x, v.y)
				} else if (!tolerateAbsence) {
					throw new IllegalStateException("Attempting to set invalid uniform \"" + uniformName + "\" on shader \"" + name + "\"")
				}
			}
		}
	}

	def setUniform(uniformName: String, v: Vec3f ) { setUniform(uniformName,v,false); }
	def setUniform(uniformName: String, v: Vec3f, tolerateAbsence: Boolean ) {
		doWhileBound {
			val stack = uniformStacks.getOrElseUpdate(uniformName,new Stack())
			if ( stack.isEmpty || stack.top != v ) {
				if ( stack.nonEmpty ) { stack.pop() }
				stack.push(v)

				val location = uniformLocations.getOrElseUpdate(uniformName, GL20.glGetUniformLocation(shaderObject, uniformName))
				if (location != -1) {
					GL20.glUniform3f(location, v.x, v.y, v.z)
				} else if (!tolerateAbsence) {
					throw new IllegalStateException("Attempting to set invalid uniform \"" + uniformName + "\" on shader \"" + name + "\"")
				}
			}
		}
	}

	def setUniform(uniformName: String, vs: Traversable[ReadVec3f] ) { setUniform(uniformName,vs,false); }
	def setUniform(uniformName: String, vs: Traversable[ReadVec3f], tolerateAbsence: Boolean ) {
		doWhileBound {
			val stack = uniformStacks.getOrElseUpdate(uniformName,new Stack())
			if ( stack.isEmpty || stack.top != vs ) {
				if ( stack.nonEmpty ) { stack.pop() }
				stack.push(vs)

				val location = uniformLocations.getOrElseUpdate(uniformName, GL20.glGetUniformLocation(shaderObject, uniformName))
				if (location != -1) {
					val arr = Array.ofDim[Float](vs.size * 3)
					var i = 0
					for (v <- vs) {
						arr(i) = v.x
						i += 1
						arr(i) = v.y
						i += 1
						arr(i) = v.z
						i += 1
					}
					GL20.glUniform3fv(location, arr)
				} else if (!tolerateAbsence) {
					throw new IllegalStateException("Attempting to set invalid uniform \"" + uniformName + "\" on shader \"" + name + "\"")
				}
			}
		}
	}

	def setUniform(uniformName: String, v: ReadVec4f) { setUniform(uniformName,v,false) }
	def setUniform(uniformName: String, v: ReadVec4f, tolerateAbsence: Boolean ) {
		doWhileBound {
			val stack = uniformStacks.getOrElseUpdate(uniformName,new Stack())
			if ( stack.isEmpty || stack.top != v ) {
				if ( stack.nonEmpty ) { stack.pop() }
				stack.push(v)

				val location = uniformLocations.getOrElseUpdate(uniformName, GL20.glGetUniformLocation(shaderObject, uniformName))
				if (location != -1) {
					GL20.glUniform4f(location, v.r, v.g, v.b, v.a)
				} else if (!tolerateAbsence) {
					throw new IllegalStateException("Attempting to set invalid uniform \"" + uniformName + "\" on shader \"" + name + "\"")
				}
			}
		}
	}

	def setUniform(uniformName: String, v: ReadMat4x4, tolerateAbsence: Boolean ) {
		doWhileBound {
			val stack = uniformStacks.getOrElseUpdate(uniformName,new Stack())
			if ( stack.isEmpty || stack.top != v ) {
				if ( stack.nonEmpty ) { stack.pop() }
				stack.push(v)

				val location = uniformLocations.getOrElseUpdate(uniformName, GL20.glGetUniformLocation(shaderObject, uniformName))
				if (location != -1) {
					val buf = GL.toTmpBuffer(v)
					GL.backing.glUniformMatrix4(location,false,buf)
				} else if (!tolerateAbsence) {
					throw new IllegalStateException("Attempting to set invalid uniform \"" + uniformName + "\" on shader \"" + name + "\"")
				}
			}
		}
	}

	def setUniformUntyped(s: String, value: Any) {
		value match {
			case f : Float => setUniform(s,f)
			case i : Int => setUniform(s,i)
			case v : Vec2f => setUniform(s,v)
			case v3 : Vec3f => setUniform(s,v3)
			case v4 : Vec4f => setUniform(s,v4)
			case _ => Noto.warn("Shader, setUniform " + s + " , unknown type : " + value)
		}
	}

	def bindAttribLocation (i: Int,name : String) {
		GL20.glBindAttribLocation(shaderObject,i,name)
		Noto.finest("Binding attribute \"" + name + "\" to index " + i + " on shader " + shaderObject + " - " + this.name)
	}
}

object Shader {
	var boundShader : Option[TShader] = None
	def unbind() {
		Shader.boundShader = None
		GL.unbindShader()
	}
}

trait TUniformProvider {
	def setUniforms (shader : Shader)
}