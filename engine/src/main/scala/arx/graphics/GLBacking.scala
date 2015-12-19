package arx.graphics

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 1/1/14
 * Time: 3:06 PM
 */

import java.nio.ByteBuffer
import java.nio.FloatBuffer
import java.nio.IntBuffer

trait GLBacking {
	def glGetTexLevelParameter(target: Int, level: Int, pname: Int, params: IntBuffer)

	def glAttachShader(program: Int, shader: Int)
	def glBindAttribLocation(program: Int, index: Int, name: String)
	def glBindBuffer(target: Int, buffer: Int)



	def glBufferData(target: Int, data: ByteBuffer, usage: Int)
	def glBufferSubData(target: Int, offset: Int, data: ByteBuffer)


	def glCompileShader(shader: Int)
	def glCreateProgram: Int
	def glCreateShader(`type`: Int): Int
	def glDeleteBuffers(buffer : Int)

	def glDeleteProgram(program: Int)
	def glDeleteShader(shader: Int)
	def glDetachShader(program: Int, shader: Int)
	def glDisableVertexAttribArray(index: Int)
	def glDrawElements(mode: Int, count: Int, `type`: Int, indices: Int)
	def glEnableVertexAttribArray(index: Int)

	def glGenBuffers() : Int
	def glGenerateMipmap(target: Int)

	def glGetAttribLocation(program: Int, name: String): Int
	def glGetBoolean(pname: Int, params: ByteBuffer)
	def glGetFloat(pname: Int, params: FloatBuffer)

	def glGetProgrami(program: Int, pname: Int) : Int
	def glGetProgramInfoLog(program: Int): String
	def glGetShaderi(shader: Int, pname: Int) : Int
	def glGetShaderInfoLog(shader: Int): String

	def glGetUniformLocation(program: Int, name: String): Int

	def glLinkProgram(program: Int)

	def glShaderSource(shader: Int, string: String)
	def glTexParameteri(target: Int, pname: Int, param: Int)

	def glUniform1f(location: Int, x: Float)
	def glUniform1i(location: Int, x: Int)
	def glUniform2f(location: Int, x: Float, y: Float)
	def glUniform2i(location: Int, x: Int, y: Int)
	def glUniform3f(location: Int, x: Float, y: Float, z: Float)
	def glUniform3i(location: Int, x: Int, y: Int, z: Int)
	def glUniform4f(location: Int, x: Float, y: Float, z: Float, w: Float)
	def glUniform4i(location: Int, x: Int, y: Int, z: Int, w: Int)
	def glUniformMatrix4(location: Int, transpose: Boolean, value: FloatBuffer)

	def glUseProgram(program: Int)
	def glValidateProgram(program: Int)
	def glVertexAttribPointer(indx: Int, size: Int, `type`: Int, normalized: Boolean, stride: Int, ptr: Int)


	def glActiveTexture(texture: Int)
	def glBindTexture(target: Int, texture: Int)
	def glBlendFunc(sfactor: Int, dfactor: Int)
	def glClear(mask: Int)
	def glClearColor(red: Float, green: Float, blue: Float, alpha: Float)
	def glColorMask(red: Boolean, green: Boolean, blue: Boolean, alpha: Boolean)
	def glCullFace(mode: Int)
	def glDeleteTextures(tex : Int)
	def glDepthFunc(func: Int)
	def glDepthMask(flag: Boolean)
	def glDisable(cap: Int)
	def glDrawArrays(mode: Int, first: Int, count: Int)
	def glEnable(cap: Int)
	def glFrontFace(mode: Int)
	def glGenTextures() : Int
	def glGetError: Int
	def glGetString(name: Int): String
	def glLineWidth(width: Float)
	def glTexImage2D(target: Int, level: Int, internalformat: Int, width: Int, height: Int, border: Int, format: Int, `type`: Int, pixels: ByteBuffer)
	def glTexSubImage2D(target: Int, level: Int, xoffset: Int, yoffset: Int, width: Int, height: Int, format: Int, `type`: Int, pixels: ByteBuffer)
	def glViewport(x: Int, y: Int, width: Int, height: Int)
}
