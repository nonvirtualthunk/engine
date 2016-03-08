package arx.graphics

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/25/12
 * Time: 2:41 PM
 * Created by nonvirtualthunk
 */

import java.util.concurrent.atomic.AtomicInteger

import arx.application.Application
import arx.application.Noto
import arx.core.datastructures.SynchronizedQueue
import arx.core.vec._
import arx.graphics.data.PointBuilder
import arx.graphics.traits.TRenderTarget
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl._
import arx.Prelude._


class AVBO(var _attribProfile : AttributeProfile) extends TRenderTarget {
	var points = new GLData(GL_FLOAT) //The GL_FLOAT doesn't actually have any particular purpose here, by the way, only used for indices
	var indices = new GLData(GL_UNSIGNED_SHORT)
	indices.shortStride = 1
	points.byteStride = _attribProfile.byteStride

	protected var _state : AtomicInteger = new AtomicInteger(VBO.Dirty)
	var deferredUntilAfterWrite = new SynchronizedQueue[() => Unit]()
	var vao = 0


	def state = _state
	def changeState(fromState : Int, toState : Int) = {
		if (_state.compareAndSet(fromState, toState)) {
			// if we moved from updating to updated we should be ok to apply all the deferred tasks
			// that were waiting until writing was finished
			if (fromState == VBO.Updating) {
				while ( deferredUntilAfterWrite.nonEmpty ) {
					deferredUntilAfterWrite.dequeueOpt() pmatch {
						case Some(task) => task.apply()
					}
				}
			}
			true
		} else {
			false
		}
	}

	def attribProfile_= ( p : AttributeProfile ) {
		_attribProfile = p
		points.byteStride = p.byteStride
	}
	def attribProfile = _attribProfile

	def bind () {
		if ( ! GL.disabled ) {
			if ( ! Application.isOpenGLThread ) { throw new IllegalStateException("Calling VBO bind on non-GL thread") }
			VAO.bind(vao)
		}
	}
	def unbind () {
		if ( ! GL.disabled ) {
			VAO.unbind()
		}
	}

	def solidify(usage: Int = GL15.GL_DYNAMIC_DRAW) {
		if ( vao == 0 ) {
			vao = GL30.glGenVertexArrays()
//			vao = GL.backing.glGenV
		}
		VAO.bind(vao)

		lcl_subSolidify(points,GL_ARRAY_BUFFER,usage)
		lcl_subSolidify(indices,GL_ELEMENT_ARRAY_BUFFER,usage)

		if ( points.numElements > 0 ) {
			setOpenGLPointers()
		}
		state.set( VBO.Solidified )
	}

	def solidify (usage: Int, numPointsToSolidify : Int , numIndicesToSolidify : Int ) {
		if ( ! GL.disabled ) {
			if ( vao == 0 ) { vao = GL30.glGenVertexArrays() }
			VAO.bind(vao)

			assert ( numPointsToSolidify <= numPoints && numIndicesToSolidify <= numIndices ,
				"Attempting to solidify more points/indices than are available : " + numPoints + "," + numIndices + "," + numPointsToSolidify + "," + numIndicesToSolidify )
			lcl_subSolidify(points,GL_ARRAY_BUFFER,usage,numPointsToSolidify)
			lcl_subSolidify(indices,GL_ELEMENT_ARRAY_BUFFER,usage,numIndicesToSolidify)

			if ( points.numElements > 0 && numPointsToSolidify > 0 ) {
				setOpenGLPointers()
			}
		}
	}

	def wouldSolidifyIfNecessary : Boolean = state.get() == VBO.Updated
	def solidifyIfNecessary(usage: Int = GL15.GL_DYNAMIC_DRAW) = {
		if ( wouldSolidifyIfNecessary ) {
			if ( state.compareAndSet(VBO.Updated,VBO.Solidifying) ) {
				solidify(usage)
				true
			} else { false }
		} else { false }
	}

	/**
	 * Clears the last updated marker, the points and indices. Does not alter any of the data
	 * already on the graphics card, nor the lastSolidifiedMarker
	 */
	def clear (){
		/* We were getting illegal arg exceptions in the byte buffers because we were being cleared while populating
		   the VBO, because the thing moved out of view before it had finished. This way we let it finish what it's
		   doing before we nuke, it potentially wastes some time, but it's much safer, and gives more reasonable
		   guarantees to the code using the VBO */
		if ( state.get() == VBO.Updating ) { deferredUntilAfterWrite.enqueue( () => this.clear() ) }
		else {
			points.clear()
			indices.clear()
		}
	}

	def drawElements (primitive: Int = GL11.GL_TRIANGLES,start: Int = 0,length: Int = -1,skipPostDraw : Boolean = false){
		if ( ! GL.disabled ) {
			val effectiveLength = if ( length == -1 ) { indices.numSolidifiedElements - start } else { length }
			if ( effectiveLength > 0 ) {
				if ( vao != 0 ) {
					VAO.bind(vao)
					GL.backing.glDrawElements(primitive,effectiveLength,indices.dataType,start * indices.byteStride)
					if ( ! skipPostDraw ) {
						VAO.unbind()
					}
				}
			}
		}
	}

	def draw (primitive: Int,start: Int = 0,length: Int = -1){
		if ( ! GL.disabled ) {
			val effectiveLength = if ( length == -1 ) { points.numSolidifiedElements - start } else { length }
			if ( effectiveLength > 0 ) {
				if ( vao != 0 ) {
					VAO.bind(vao)
					GL.backing.glDrawArrays(primitive,start,effectiveLength)
					VAO.unbind()
				}
			}
		}
	}

	def setOpenGLPointers() {
		if ( VBO.boundArrayBuffer != 0 ) {
			var i = _attribProfile.attributes.length - 1;while ( i >= 0 ) {
				VBO.enableVertexAttribArray(i)
				val attribute = _attribProfile.attributes(i)
				val normalize = attribute.normalize && (attribute.dataType == GL_BYTE || attribute.dataType == GL_UNSIGNED_BYTE || attribute.dataType == GL_SHORT || attribute.dataType == GL_UNSIGNED_SHORT ||
										attribute.dataType == GL_INT || attribute.dataType == GL_UNSIGNED_INT)

				if ( ! attribute.rawInteger ) {
					GL.backing.glVertexAttribPointer(i,attribute.size,attribute.dataType,normalize,_attribProfile.byteStride,attribute.byteOffset)
				} else {
					GL30.glVertexAttribIPointer(i,attribute.size,attribute.dataType,_attribProfile.byteStride,attribute.byteOffset)
				}
			i -= 1}
		} else {
			Noto.severeError("Attribute vbos must be bound to be used, bound : " + VBO.boundArrayBuffer + " name : " + points.name)
		}
	}

	def lcl_subSolidify(data: GLData , bufferType: Int, usage: Int, limit : Int = -1){
		if ( state.get() == VBO.Updating ) { throw new IllegalStateException("Writing active while solidifying") }
		val tmpBuffer = data.rawData.asReadOnlyBuffer()
		val effectiveLimit = if ( limit == -1 ) { data.numElements * data.byteStride } else { limit * data.byteStride } //convert the numElements realm limit into byte limit

		if ( effectiveLimit > 0 ) {
			tmpBuffer.position(0)
			tmpBuffer.limit(effectiveLimit)

			if ( data.name == 0 ){
				data.name = GL.backing.glGenBuffers()
			}
			VBO.bindBuffer(bufferType,data.name)

			GL.backing.glBufferData(bufferType,tmpBuffer,usage)
		}

		data.numSolidifiedElements = effectiveLimit / data.byteStride //account for the possibility of a supplied limit
	}

	def unsolidify () {
		lcl_subUnsolidify(points)
		lcl_subUnsolidify(indices)
		// TODO: should this change the state to Dirty?
	}

	def lcl_subUnsolidify(data: GLData) {
		if ( data.name != 0 ){
			GL.vbosToDestroy synchronized {
				GL.vbosToDestroy.enqueue(data.name)
				data.numSolidifiedElements = 0
				data.name = 0
			}
		}
	}

	def isSolidified = points.name != 0 || indices.name != 0
	def numPoints: Int = points.numElements
	def numPoints_= (n: Int){
		points.resizeElements(n)
	}
	def numIndices: Int = indices.numElements
	def numIndices_= (n: Int){
		indices.resizeElements(n)
	}
	def numPointsHint (n: Int){
		points.reserveElements(n)
	}
	def numIndicesHint (n: Int){
		indices.reserveElements(n)
	}


	def setPoint(n : Int, buf : PointBuilder): Unit = {
		points.rawData.position(n * points.byteStride)
		buf.bytes.rewind()
		points.rawData.put(buf.bytes)
	}
	def setA (attribIndex: Int,n : Int, v : ReadVec2f) { points.setV2(n,_attribProfile.attributes(attribIndex).floatOffset,v) }
	def setA (attribIndex: Int,n : Int, v : ReadVec3f) { points.setV3(n,_attribProfile.attributes(attribIndex).floatOffset,v) }
	def setA (attribIndex: Int,n : Int, v : ReadVec4f) { points.setV4(n,_attribProfile.attributes(attribIndex).floatOffset,v) }
	def setA (attribIndex: Int,n : Int, x : Float ) { points.set(n,_attribProfile.attributes(attribIndex).floatOffset,x) }
	def setA (attribIndex: Int,n : Int, x : Float, y : Float) { points.set(n,_attribProfile.attributes(attribIndex).floatOffset,x,y) }
	def setA (attribIndex: Int,n : Int, x : Float, y : Float, z : Float) { points.set(n,_attribProfile.attributes(attribIndex).floatOffset,x,y,z) }
	def setAs (attribIndex: Int,n : Int, x : Short, y : Short, z : Short) { points.setS(n,_attribProfile.attributes(attribIndex).shortOffset,x,y,z) }
	def setAs (attribIndex: Int,n : Int, x : Short, y : Short) { points.setS(n,_attribProfile.attributes(attribIndex).shortOffset,x,y) }
	def setA (attribIndex: Int,n : Int, r : Float, g : Float, b : Float, a : Float) { points.set(n,_attribProfile.attributes(attribIndex).floatOffset,r,g,b,a) }

	def setAb (attribIndex: Int,n : Int, b : Byte) { points.setB(n,_attribProfile.attributes(attribIndex).byteOffset,b) }
	def setAb (attribIndex: Int,n : Int, r : Byte, g : Byte, b : Byte, a : Byte) { points.setB(n,_attribProfile.attributes(attribIndex).byteOffset,r,g,b,a) }
	def setAbpacked (attribIndex: Int,n : Int, packed : Int) { points.setPacked(n,_attribProfile.attributes(attribIndex).byteOffset,packed) }
	def setAbf (attribIndex: Int,n : Int, r : Float, g : Float ) {
		points.setB(n,_attribProfile.attributes(attribIndex).byteOffset,(r*255).toByte,(g*255).toByte) }
	def setAbf (attribIndex: Int,n : Int, r : Float, g : Float, b : Float, scale : Int) {
		points.setB(n,_attribProfile.attributes(attribIndex).byteOffset,(r*scale).toByte,(g*scale).toByte,(b*scale).toByte) }
	def setAbf (attribIndex: Int,n : Int, r : Float, g : Float, b : Float, a : Float) {
		points.setB(n,_attribProfile.attributes(attribIndex).byteOffset,(r*255).toByte,(g*255).toByte,(b*255).toByte,(a*255).toByte) }
	def setAbf (attribIndex: Int,n : Int, r : Float, g : Float, b : Float, a : Float, scale : Int) {
		points.setB(n,_attribProfile.attributes(attribIndex).byteOffset,(r*scale).toByte,(g*scale).toByte,(b*scale).toByte,(a*scale).toByte) }

	def attributeIndex ( name : String ) = _attribProfile.attributesByName(name)

	@inline
	def setI (n: Int, i: Short) {
		if ( indices.dataType == GL_UNSIGNED_SHORT ) {
			indices.set(n,i)
		} else {
			indices.set(n,i.toInt)
		}
	}
	@inline
	def setI (n: Int, i: Int) {
		if ( indices.dataType == GL_UNSIGNED_SHORT ) {
			if ( i > (Short.MaxValue << 1) ) { throw new IllegalStateException("Index out of range in VBO : " + i) }
			indices.set(n,i.toShort)
		} else {
			indices.set(n,i)
		}
	}

	def setIQuad ( ii : Int , vi : Int ) {
		vbo.setI(ii + 0, vi + 0)
		vbo.setI(ii + 1, vi + 1)
		vbo.setI(ii + 2, vi + 2)

		vbo.setI(ii + 3, vi + 2)
		vbo.setI(ii + 4, vi + 3)
		vbo.setI(ii + 5, vi + 0)
	}
	def setIQuadReverse ( ii : Int , vi : Int ) {
		vbo.setI(ii + 5, vi + 0)
		vbo.setI(ii + 4, vi + 1)
		vbo.setI(ii + 3, vi + 2)

		vbo.setI(ii + 2, vi + 2)
		vbo.setI(ii + 1, vi + 3)
		vbo.setI(ii + 0, vi + 0)
	}

	def isEmpty = (! isSolidified && numPoints == 0 && numIndices == 0) || (isSolidified && points.numSolidifiedElements == 0 && indices.numSolidifiedElements == 0)
	def nonEmpty = ! isEmpty

	//TRenderTarget implementation
	def vbo : AVBO = this
	def incrementVertexOffset ( n : Int ) = {
		val r = numPoints
		numPoints = r + n
		r
	}
	def incrementIndexOffset ( n : Int ) = {
		val r = numIndices
		numIndices = r + n
		r
	}
	def vertexOffset = numPoints
	def indexOffset = numIndices

	def useIntIndices () {
		indices = new GLData(GL_UNSIGNED_INT)
		indices.intStride = 1
	}
}


case class AttribInfo(size : Int, dataType : Int, byteOffset : Int , name : String, var normalize : Boolean) {
	val floatOffset = byteOffset >> 2
	val shortOffset = byteOffset >> 1
	var rawInteger = false
}

object AVBO {
	val Clean = VBO.Clean
	val Dirty = VBO.Dirty
	val Updating = VBO.Updating
	val Updated = VBO.Updated
	val Solidified = VBO.Solidified
}