package arx.graphics

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/1/14
 * Time: 3:12 PM
 */

import java.nio.ByteBuffer
import java.nio.FloatBuffer
import java.nio.IntBuffer

import org.lwjgl.opengl._

object LWJGL3Backing extends GLBacking {
	def glGetTexLevelParameter(target: Int, level: Int, pname: Int, params: IntBuffer) = GL11.glGetTexLevelParameteriv(target,level,pname,params)
	
	def glAttachShader(program: Int, shader: Int): Unit = GL20.glAttachShader(program,shader)

	def glBindAttribLocation(program: Int, index: Int, name: String): Unit = GL20.glBindAttribLocation(program,index,name)

	def glBindBuffer(target: Int, buffer: Int): Unit = GL15.glBindBuffer(target,buffer)

	def glBufferData(target: Int, data: ByteBuffer, usage: Int): Unit = GL15.glBufferData(target,data,usage)

	def glBufferSubData(target: Int, offset: Int, data: ByteBuffer): Unit = GL15.glBufferSubData(target,offset,data)

	def glCompileShader(shader: Int): Unit = GL20.glCompileShader(shader)

	def glCreateProgram: Int = GL20.glCreateProgram()

	def glCreateShader(`type`: Int): Int = GL20.glCreateShader(`type`)

	def glDeleteBuffers(buffer : Int): Unit = GL15.glDeleteBuffers(buffer)

	def glDeleteProgram(program: Int): Unit = GL20.glDeleteProgram(program)

	def glDeleteShader(shader: Int): Unit = GL20.glDeleteShader(shader)

	def glDetachShader(program: Int, shader: Int): Unit = GL20.glDetachShader(program,shader)

	def glDisableVertexAttribArray(index: Int): Unit = GL20.glDisableVertexAttribArray(index)

	def glDrawElements(mode: Int, count: Int, `type`: Int, indices: Int): Unit = GL11.glDrawElements(mode,count,`type`,indices)

	def glEnableVertexAttribArray(index: Int): Unit = GL20.glEnableVertexAttribArray(index)

	def glGenBuffers(): Int = GL15.glGenBuffers()

	def glGenerateMipmap(target: Int): Unit = GL30.glGenerateMipmap(target)

	def glGetAttribLocation(program: Int, name: String): Int = GL20.glGetAttribLocation(program,name)

	def glGetBoolean(pname: Int, params: ByteBuffer): Unit = GL11.glGetBooleanv(pname,params)

	def glGetFloat(pname: Int, params: FloatBuffer): Unit = GL11.glGetFloatv(pname,params)

	def glGetProgrami(program: Int, pname: Int): Int = GL20.glGetProgrami(program,pname)

	def glGetProgramInfoLog(program: Int): String = GL20.glGetProgramInfoLog(program,1000)

	def glGetShaderi(shader: Int, pname: Int): Int = GL20.glGetShaderi(shader,pname)

	def glGetShaderInfoLog(shader: Int): String = GL20.glGetShaderInfoLog(shader,1000)

	def glGetUniformLocation(program: Int, name: String): Int = GL20.glGetUniformLocation(program,name)

	def glLinkProgram(program: Int): Unit = GL20.glLinkProgram(program)

	def glShaderSource(shader: Int, string: String): Unit = GL20.glShaderSource(shader,string)

	def glTexParameteri(target: Int, pname: Int, param: Int): Unit = GL11.glTexParameteri(target,pname,param)

	def glUniform1f(location: Int, x: Float): Unit = GL20.glUniform1f(location,x)
	def glUniform1i(location: Int, x: Int): Unit = GL20.glUniform1i(location,x)
	def glUniform2f(location: Int, x: Float, y: Float): Unit = GL20.glUniform2f(location,x,y)
	def glUniform2i(location: Int, x: Int, y: Int): Unit = GL20.glUniform2i(location,x,y)
	def glUniform3f(location: Int, x: Float, y: Float, z: Float): Unit = GL20.glUniform3f(location,x,y,z)
	def glUniform3i(location: Int, x: Int, y: Int, z: Int): Unit = GL20.glUniform3i(location,x,y,z)
	def glUniform4f(location: Int, x: Float, y: Float, z: Float, w: Float): Unit = GL20.glUniform4f(location,x,y,z,w)
	def glUniform4i(location: Int, x: Int, y: Int, z: Int, w: Int): Unit = GL20.glUniform4i(location,x,y,z,w)
	def glUniformMatrix4(location: Int, transpose: Boolean, value: FloatBuffer): Unit = GL20.glUniformMatrix4fv(location,transpose,value)

	def glUseProgram(program: Int): Unit = GL20.glUseProgram(program)

	def glValidateProgram(program: Int): Unit = GL20.glValidateProgram(program)

	def glVertexAttribPointer(indx: Int, size: Int, `type`: Int, normalized: Boolean, stride: Int, ptr: Int): Unit = GL20.glVertexAttribPointer(indx,size,`type`,normalized,stride,ptr)

	def glActiveTexture(texture: Int): Unit = GL13.glActiveTexture(texture)

	def glBindTexture(target: Int, texture: Int): Unit = GL11.glBindTexture(target,texture)

	def glBlendFunc(sfactor: Int, dfactor: Int): Unit = GL11.glBlendFunc(sfactor,dfactor)

	def glClear(mask: Int): Unit = GL11.glClear(mask)

	def glClearColor(red: Float, green: Float, blue: Float, alpha: Float): Unit = GL11.glClearColor(red,green,blue,alpha)

	def glColorMask(red: Boolean, green: Boolean, blue: Boolean, alpha: Boolean): Unit = GL11.glColorMask(red,green,blue,alpha)

	def glCullFace(mode: Int): Unit = GL11.glCullFace(mode)

	def glDeleteTextures(tex : Int): Unit = GL11.glDeleteTextures(tex)

	def glDepthFunc(func: Int): Unit = GL11.glDepthFunc(func)

	def glDepthMask(flag: Boolean): Unit = GL11.glDepthMask(flag)

	def glDisable(cap: Int): Unit = GL11.glDisable(cap)

	def glDrawArrays(mode: Int, first: Int, count: Int): Unit = GL11.glDrawArrays(mode,first,count)

	def glEnable(cap: Int): Unit = GL11.glEnable(cap)

	def glFrontFace(mode: Int): Unit = GL11.glFrontFace(mode)

	def glGenTextures(): Int = GL11.glGenTextures()

	def glGetError: Int = GL11.glGetError()

	def glGetString(name: Int): String = GL11.glGetString(name)

	def glLineWidth(width: Float): Unit = GL11.glLineWidth(width)

	def glTexImage2D(target: Int, level: Int, internalformat: Int, width: Int, height: Int, border: Int, format: Int, `type`: Int, pixels: ByteBuffer): Unit = {
		GL11.glTexImage2D(target,level,internalformat,width,height,border,format,`type`,pixels)
	}
	
	def glTexSubImage2D(target: Int, level: Int, xoffset: Int, yoffset: Int, width: Int, height: Int, format: Int, `type`: Int, pixels: ByteBuffer): Unit = {
		GL11.glTexSubImage2D(target,level,xoffset,yoffset,width,height,format,`type`,pixels)
	}

	def glViewport(x: Int, y: Int, width: Int, height: Int): Unit = GL11.glViewport(x,y,width,height)
}
