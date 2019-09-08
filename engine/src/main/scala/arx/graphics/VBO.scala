package arx.graphics

import java.nio.ByteBuffer
import java.nio.FloatBuffer
import java.nio.ShortBuffer

import arx.application.Application
import arx.core.vec._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl._


object VBO{
	val TexCoordArray = 0
	val ColorArray = 1
	val NormalArray = 2
	val VertexArray = 3

	val Clean = 0
	val Dirty = 1
	val Updating = 2
	val Updated = 3
	val Solidifying = 4
	val Solidified = 0

	var boundArrayBuffer = 0
	var boundIndexBuffer = 0

	def unbind () {
		if ( ! GL.disabled ) {
			if ( ! Application.isOpenGLThread ) { throw new IllegalStateException("Calling VBO bind on non-GL thread") }
			VBO.bindBuffer(GL_ARRAY_BUFFER,0)
			VBO.bindBuffer(GL_ELEMENT_ARRAY_BUFFER,0)
		}
	}

	def bindBuffer ( bufferType : Int , name : Int ) {
		if ( ! GL.disabled ) {
			if ( bufferType == GL_ARRAY_BUFFER ) {
				if ( boundArrayBuffer != name ) {
					glBindBuffer(bufferType,name)
					boundArrayBuffer = name
				}
			} else {
				if ( boundIndexBuffer != name ) {
					glBindBuffer(bufferType,name)
					boundIndexBuffer = name
				}
			}
		}
	}

	val vertexAttribArrayStates = new Array[Boolean](20)
	//TODO: Re-enable this check...it should work I would think
	def enableVertexAttribArray ( i : Int ) {
//		if ( ! vertexAttribArrayStates(i) ) {
			GL20.glEnableVertexAttribArray(i)
			vertexAttribArrayStates(i) = true
//		}
	}
	def disableVertexAttribArray ( i : Int ) {
//		if ( vertexAttribArrayStates(i) ) {
			GL20.glDisableVertexAttribArray(i)
			vertexAttribArrayStates(i) = false
//		}
	}
}

object VAO {
	var currentlyBound = 0
	def bind ( vao : Int ) {
		if ( currentlyBound != vao ) {
			currentlyBound = vao
			GL30.glBindVertexArray(vao)
		}
	}
	def unbind ( ) {
		if ( currentlyBound != 0 ) {
			currentlyBound = 0
			GL30.glBindVertexArray(0)
		}
	}
}

class VBOArrayTypes(val offset: Int){
	val byteOffset = offset * 4
}

class GLData(var dataType: Int){
	var rawData: ByteBuffer = GL.getBumperBuffer
	var floatData: FloatBuffer = rawData.asFloatBuffer()
	var shortData: ShortBuffer = rawData.asShortBuffer()
	var intData = rawData.asIntBuffer
	var name: Int = 0
	var byteStride_v: Int = 0
	var floatStride_v: Int = 0
	var numElements: Int = 0
	var numSolidifiedElements: Int = 0

	// We can't do shifting for strides to avoid multiplications in the set calls, these aren't guaranteed
	// to land only on powers of two. 12 floats is a totally valid stride, but would be 48 bytes, no po2 there
	def byteStride: Int = byteStride_v
	def byteStride_=(n: Int){ byteStride_v = n;floatStride_v = n / 4 }
	def intStride: Int = floatStride_v
	def intStride_=(n: Int){ byteStride = n * 4;floatStride_v = n }
	def shortStride_=(n:Int) { byteStride = n * 2; }
	def shortStride: Int = (floatStride_v << 1)
	def floatStride_=(n: Int){ byteStride = n * 4;floatStride_v = n }
	def floatStride: Int = floatStride_v

	def resizeElements (n: Int){
		reserveElements(n)
		numElements = n
	}

	def reserveElements (n: Int) {
		synchronized {
			val requiredCapacity = n * byteStride
			if ( requiredCapacity > rawData.capacity() ){
				var newCapacity = scala.math.max(2,rawData.capacity)
				while ( newCapacity < requiredCapacity ){ newCapacity = (newCapacity * 2).toInt }
				var newData: ByteBuffer = null
				try{
					newData = GL.createByteBuffer(newCapacity)
				}
				catch{
					case e: OutOfMemoryError =>
						println("OOM, trying a gc")
						println("\tError while attempting to allocate \"" + (newCapacity / (1 << 20).doubleValue) + "\" MB")
						System.gc()
						newData = GL.createByteBuffer(newCapacity)
				}
				if ( rawData != null ) {
					rawData.rewind()
					newData.rewind()
					if ( n < numElements ) { rawData.limit(n * byteStride) }
					else{ rawData.limit(numElements * byteStride) }
					newData.put(rawData)
				}
				newData.rewind()
				newData.limit(newData.capacity)
				GL.freeByteBuffer(rawData)
				rawData = newData
				floatData = rawData.asFloatBuffer()
				shortData = rawData.asShortBuffer()
				intData = rawData.asIntBuffer
			}
		}
	}

	def clear () {
		GL.freeByteBuffer(rawData)
		rawData = GL.getBumperBuffer
		floatData = rawData.asFloatBuffer()
		shortData = rawData.asShortBuffer()
		intData = rawData.asIntBuffer
		numElements = 0
	}

	def softClear(): Unit = {
		rawData.rewind()
		rawData.limit(rawData.capacity)
		numElements = 0
	}

	def getAsVec4 (n: Int,offset: Int): ReadVec4f = {
		floatData.position(offset + n * floatStride)
		Vec4f(floatData.get(),floatData.get(),floatData.get(),floatData.get())
	}
	def getAsVec3 (n: Int,offset: Int): ReadVec3f = {
		floatData.position(offset + n * floatStride)
		Vec3f(floatData.get(),floatData.get(),floatData.get())
	}
	def getAsVec2 (n: Int,offset: Int): ReadVec2f = {
		floatData.position(offset + n * floatStride)
		Vec2f(floatData.get(),floatData.get())
	}
	def getAsInt (n: Int): Int = shortData.get(n)
	def setV4(n: Int, offset: Int, v: ReadVec4f) {
		val o = offset + n * floatStride
		floatData.put(o+0,v.r);floatData.put(o+1,v.g);floatData.put(o+2,v.b);floatData.put(o+3,v.a)
	}
	def setV3(n: Int, offset: Int, v: ReadVec3f) {
		floatData.position(offset + n * floatStride)
		floatData.put(v.r);floatData.put(v.g);floatData.put(v.b);
	}
	def setV2(n: Int, offset: Int, v: ReadVec2f) {
		floatData.position(offset + n * floatStride)
		floatData.put(v.x);floatData.put(v.y);
	}
	def set (n: Int,offset: Int,r: Float,g: Float,b: Float,a: Float) {
		floatData.position(offset + n * floatStride)
		floatData.put(r);floatData.put(g);floatData.put(b);floatData.put(a)
	}
	def setB ( n : Int , byteOffset : Int, r: Byte, g : Byte ) {
		rawData.position(byteOffset + n * byteStride)
		rawData.put(r);rawData.put(g)
	}
	def setB ( n : Int , byteOffset : Int, r: Byte, g : Byte, b : Byte, a : Byte) {
		rawData.position(byteOffset + n * byteStride)
		rawData.put(r);rawData.put(g);rawData.put(b);rawData.put(a)
	}
	def setB ( n : Int , byteOffset : Int, r: Byte, g : Byte, b : Byte) {
		rawData.position(byteOffset + n * byteStride)
		rawData.put(r);rawData.put(g);rawData.put(b)
	}
	def setB ( n : Int , byteOffset : Int, b: Byte) {
		rawData.put(byteOffset + n * byteStride,b)
	}
	def setPacked ( n : Int , byteOffset : Int, packed: Int ){
		rawData.position(byteOffset + n * byteStride)
		rawData.put((packed&0x000000ff).toByte)
		rawData.put(((packed&0x0000ff00) >>> 8).toByte)
		rawData.put(((packed&0x00ff0000) >>> 16).toByte)
		rawData.put(((packed&0xff000000) >>> 24).toByte)
	}
	def set (n: Int,offset: Int,i : Int,v : Float) {
		floatData.position(offset + n * floatStride + i)
		floatData.put(v)
	}
	def set (n: Int,offset: Int,x: Float,y: Float,z: Float) {
		floatData.position(offset + n * floatStride)
		floatData.put(x);floatData.put(y);floatData.put(z)
	}
	def setS (n: Int,offset: Int,x: Short,y: Short,z: Short) {
		shortData.position(offset + n * shortStride)
		shortData.put(x);shortData.put(y);shortData.put(z)
	}
	def setS (n: Int,offset: Int,x: Short,y: Short) {
		shortData.position(offset + n * shortStride)
		shortData.put(x);shortData.put(y)
	}
	def set (n: Int,offset: Int,x: Float,y: Float) {
		floatData.position(offset + n * floatStride)
		floatData.put(x);floatData.put(y);
	}
	def set (n: Int, offset : Int, f : Float ){
		floatData.put(offset + n * floatStride,f)
	}
	def set (n: Int, i : Short ){
		shortData.put(n,i)
	}
	def set (n: Int, i : Int ){
		intData.put(n,i)
	}
}