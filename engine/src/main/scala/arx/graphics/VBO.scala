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
	val TexCoord = 1 << TexCoordArray
	val Color = 1 << ColorArray
	val Normal = 1 << NormalArray
	val Vertex = 1 << VertexArray
	val TCNV = Vertex|Color|Normal|TexCoord

	val NumArrayTypes = 4

//	def defineNewArrayType ( size : Int , dataType : Int ) : Int


	var subArrayFloatStrides = Map( TexCoordArray -> 2 , ColorArray -> 4 , NormalArray -> 3, VertexArray -> 3 )

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

	def unsetArrayPointers () {
		for ( i <- 0 until VBO.NumArrayTypes ){
			i match {
				case VBO.TexCoordArray => disableClientState(GL_TEXTURE_COORD_ARRAY)
				case VBO.ColorArray => disableClientState(GL_COLOR_ARRAY)
				case VBO.NormalArray => disableClientState(GL_NORMAL_ARRAY)
				case VBO.VertexArray => disableClientState(GL_VERTEX_ARRAY)
				case _ => println("invalid i")
			}
		}
	}

	var clientState = Map[Int,Boolean]()
	def enableClientState( i : Int ) {
		var cur = clientState.getOrElse(i,false)
		if ( ! cur ) {
			glEnableClientState(i)
			cur = true
		}
		clientState += i -> cur
	}
	def disableClientState ( i : Int ) {
		var cur = clientState.getOrElse(i,true)
		if ( cur ) {
			glDisableClientState(i)
			cur = false
		}
		clientState += i -> cur
	}
	val vertexAttribArrayStates = new Array[Boolean](20)
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

	//var buffersToDelete = List[GLData]()
	var buffersToDelete = List[Int]()

	var solidifiedSizes = List[Int]()
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
	def set (n: Int,offset: Int,v: ReadVec4f) {
		val o = offset + n * floatStride
		floatData.put(o+0,v.r);floatData.put(o+1,v.g);floatData.put(o+2,v.b);floatData.put(o+3,v.a)
	}
	def set (n: Int,offset: Int,v: ReadVec3f) {
		floatData.position(offset + n * floatStride)
		floatData.put(v.r);floatData.put(v.g);floatData.put(v.b);
	}
	def set (n: Int,offset: Int,v: ReadVec2f) {
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
/*
Sync'd: name, solidified length
 */
//class VBO(activeArrays_arg:Int) extends TVBO {
//	var points = new GLData(GL_FLOAT)
//	var indices = new GLData(GL_UNSIGNED_SHORT)
//	indices.shortStride = 1
//
//	var markedForUpdate: Boolean = false
//	var lastUpdatedMarker: Long = 0
//	var lastSolidifiedMarker: Long = 0
//	var _writingActive: Boolean = false
//	var deferredUntilAfterWrite = new SynchronizedQueue[() => Unit]()
//	var unsolidifiedLevelOfDetail : Int = -1
//	var solidifiedLevelOfDetail : Int = -1
//	var vao = 0
//
//	def effectiveLevelOfDetail = if ( solidifiedLevelOfDetail == -1 ) { unsolidifiedLevelOfDetail } else { solidifiedLevelOfDetail }
//
//	def writingActive : Boolean = _writingActive
//	def writingActive_= ( b : Boolean ) {
//		if ( _writingActive && ! b ) {
//			_writingActive = b
//			while ( deferredUntilAfterWrite.nonEmpty ) { (deferredUntilAfterWrite.dequeue())() }
//		} else { _writingActive = b }
//	}
//
//	var _activeArrays = 0
//	def activeArrays = _activeArrays
//	private val subArrayInfo = new Array[VBOArrayTypes](VBO.NumArrayTypes)
//
//	def activeArrays_= ( i : Int ) {
//		subArrayInfo synchronized {
//			_activeArrays = i
//			points.byteStride = 0
//
//			for ( i <- 0 until VBO.NumArrayTypes ) { subArrayInfo(i) = null }
//
//			for ( i <- 0 until VBO.NumArrayTypes ) {
//				if ((activeArrays & (1 << i)) != 0) {
//					subArrayInfo(i) = new VBOArrayTypes(points.floatStride)
//					points.byteStride = points.byteStride + VBO.subArrayFloatStrides(i) * 4
//				}
//			}
//		}
//	}
//	activeArrays = activeArrays_arg
//
//
//	def unbind () {
//		Prelude.posit ( VBO.boundArrayBuffer == points.name && VBO.boundIndexBuffer == indices.name , "Called unbind on VBO that is not the one currently bound, not invalid, but wrong" )
//		VBO.unbind()
//		VAO.unbind()
//	}
//
//	def solidify (usage: Int = GL_DYNAMIC_DRAW) {
//		if ( vao == 0 ) { vao = GL30.glGenVertexArrays() }
//		VAO.bind(vao)
//
//		lcl_subSolidify(points,GL_ARRAY_BUFFER,usage)
//		lcl_subSolidify(indices,GL_ELEMENT_ARRAY_BUFFER,usage)
//
//		if ( points.numElements > 0 ) {
//			setOpenGLPointers()
//		}
//	}
//	def solidify (usage: Int, numPointsToSolidify : Int , numIndicesToSolidify : Int ) {
//		if ( ! GL.disabled ) {
//			if ( vao == 0 ) { vao = GL30.glGenVertexArrays() }
//			VAO.bind(vao)
//
//			assert ( numPointsToSolidify <= numPoints && numIndicesToSolidify <= numIndices ,
//				"Attempting to solidify more points/indices than are available : " + numPoints + "," + numIndices + "," + numPointsToSolidify + "," + numIndicesToSolidify )
//			lcl_subSolidify(points,GL_ARRAY_BUFFER,usage,numPointsToSolidify)
//			lcl_subSolidify(indices,GL_ELEMENT_ARRAY_BUFFER,usage,numIndicesToSolidify)
//
//			if ( points.numElements > 0 && numPointsToSolidify > 0 ) {
//				setOpenGLPointers()
//			}
//		}
//	}
//
//	def wouldSolidifyIfNecessary : Boolean =
//		(lastUpdatedMarker > lastSolidifiedMarker ||
//			(unsolidifiedLevelOfDetail >= 0 && unsolidifiedLevelOfDetail != solidifiedLevelOfDetail)) &&
//			! markedForUpdate &&
//			! writingActive
//	def solidifyIfNecessary (usage: Int = GL_DYNAMIC_DRAW) : Boolean = {
//		if ( wouldSolidifyIfNecessary ) {
//			lastSolidifiedMarker = lastUpdatedMarker
//			solidifiedLevelOfDetail = unsolidifiedLevelOfDetail
//			solidify(usage)
//			true
//		} else { false }
//	}
//
//	def enableVertexAttributeArrays (){}
//
//	def setOpenGLPointers (){
//		subArrayInfo synchronized {
//			try{
//				if ( VBO.boundArrayBuffer != 0 ) {
//					for ( i <- 0 until VBO.NumArrayTypes ){
//						if ( (activeArrays & (1 <<i)) != 0 ){
//							i match {
//								case VBO.TexCoordArray =>
//									VBO.enableClientState(GL_TEXTURE_COORD_ARRAY)
//									glTexCoordPointer(2,GL_FLOAT,points.byteStride,subArrayInfo(i).byteOffset)
//								case VBO.ColorArray =>
//									VBO.enableClientState(GL_COLOR_ARRAY)
//									glColorPointer(4,GL_FLOAT,points.byteStride,subArrayInfo(i).byteOffset)
//								case VBO.NormalArray =>
//									VBO.enableClientState(GL_NORMAL_ARRAY)
//									glNormalPointer(GL_FLOAT,points.byteStride,subArrayInfo(i).byteOffset)
//								case VBO.VertexArray =>
//									VBO.enableClientState(GL_VERTEX_ARRAY)
//									glVertexPointer(3,GL_FLOAT,points.byteStride,subArrayInfo(i).byteOffset)
//								case _ => println("invalid i")
//							}
//						}
//					}
//				} else {
//					for ( i <- 0 until VBO.NumArrayTypes ){
//						if ( (activeArrays & (1 <<i)) != 0 ){
//							val data = points.rawData.asFloatBuffer
//							data.position(subArrayInfo(i).offset)
//							i match {
//								case VBO.TexCoordArray =>
//									VBO.enableClientState(GL_TEXTURE_COORD_ARRAY)
//									glTexCoordPointer(2,GL_FLOAT,data)
//								case VBO.ColorArray =>
//									VBO.enableClientState(GL_COLOR_ARRAY)
//									glColorPointer(4,GL_FLOAT,data)
//								case VBO.NormalArray =>
//									VBO.enableClientState(GL_NORMAL_ARRAY)
//									glNormalPointer(GL_FLOAT,data)
//								case VBO.VertexArray =>
//									VBO.enableClientState(GL_VERTEX_ARRAY)
//									glVertexPointer(3,GL_FLOAT,data)
//								case _ => println("invalid i")
//							}
//						}
//					}
//				}
//			}
//			catch {
//				case e: OpenGLException =>
//					println("Opengl exception")
//					e.printStackTrace()
//				case e : Throwable =>
//					println("Something other exception encountered while setting opengl pointers")
//					e.printStackTrace()
//			}
//		}
//	}
//	def unsetOpenGLPointers (){
//		subArrayInfo synchronized {
//			for ( i <- 0 until VBO.NumArrayTypes ){
//				if ( (activeArrays & (1 <<i)) != 0 ){
//					i match {
//						case VBO.TexCoordArray => VBO.disableClientState(GL_TEXTURE_COORD_ARRAY)
//						case VBO.ColorArray => VBO.disableClientState(GL_COLOR_ARRAY)
//						case VBO.NormalArray => VBO.disableClientState(GL_NORMAL_ARRAY)
//						case VBO.VertexArray => VBO.disableClientState(GL_VERTEX_ARRAY)
//						case _ => println("invalid i")
//					}
//				}
//			}
//		}
//	}
//
//	/**
//	 * Clears the last updated marker, the points and indices. Does not alter any of the data
//	 * already on the graphics card, nor the lastSolidifiedMarker
//	 */
//	def clear (){
//		/* We were getting illegal arg exceptions in the byte buffers because we were being cleared while populating
//		   the VBO, because the thing moved out of view before it had finished. This way we let it finish what it's
//		   doing before we nuke, it potentially wastes some time, but it's much safer, and gives more reasonable
//		   guarantees to the code using the VBO */
//		if ( writingActive ) { deferredUntilAfterWrite.enqueue( () => this.clear() ) }
//		else {
////			points synchronized {
//				points.clear()
////			}
////			indices synchronized {
//				indices.clear()
////			}
//			lastUpdatedMarker = 0
//			unsolidifiedLevelOfDetail = -1
//		}
//	}
//
//	def preDraw (){
//		bind()
//		setOpenGLPointers()
//	}
//
//	def mainDrawElements (primitive: Int,start: Int,length: Int){
//		if ( VBO.boundIndexBuffer != 0 ) {
//			GL.elementsDrawnThisFrame
//			glDrawElements(primitive,length,indices.dataType,start * indices.byteStride)
//		} else {
//			val tmpBuffer = indices.shortData.asReadOnlyBuffer()
//			tmpBuffer.position(start)
//			tmpBuffer.limit(math.min(start + length,tmpBuffer.capacity))
//			glDrawElements(primitive,indices.shortData)
//		}
//	}
//
//	def mainDraw (primitive: Int,start: Int,length: Int){
//		glDrawArrays(primitive,start,length)
//	}
//
//	def postDraw (){
//		unsetOpenGLPointers()
//		unbind()
//	}
//
//	def drawElements (primitive: Int,start: Int = 0,length: Int = -1,skipPostDraw : Boolean = false){
//		if ( ! GL.disabled ) {
//			val effectiveLength = if ( length == -1 ) { indices.numSolidifiedElements - start } else { length }
//			if ( effectiveLength > 0 ) {
////				preDraw()
////				mainDrawElements(primitive,start,effectiveLength)
////				if ( ! skipPostDraw ) { postDraw() }
//
//				if ( vao != 0 ) {
//					VAO.bind(vao)
////					bind()
////					enableVertexAttributeArrays()
//					glDrawElements(primitive,effectiveLength,indices.dataType,start * indices.byteStride)
////					unsetOpenGLPointers()
////					unbind()
//					VAO.unbind()
//				}
//			}
//		}
//	}
//
//	def draw (primitive: Int,start: Int = 0,length: Int = -1){
//		if ( ! GL.disabled ) {
//			val effectiveLength = if ( length == -1 ) { points.numSolidifiedElements - start } else { length }
//			if ( effectiveLength > 0 ) {
////				preDraw()
////				mainDraw(primitive,start,effectiveLength)
////				postDraw()
//
//				if ( vao != 0 ) {
//					bind()
//					enableVertexAttributeArrays()
//					glDrawArrays(primitive,start,effectiveLength)
//					unsetOpenGLPointers()
//				}
//			}
//		}
//	}
//
//	def lcl_subSolidify(data: GLData , bufferType: Int, usage: Int, limit : Int = -1){
////		data.synchronized {
//			if ( writingActive ) { throw new IllegalStateException("Writing active while solidifying") }
//			val tmpBuffer = data.rawData.asReadOnlyBuffer()
//			val effectiveLimit = if ( limit == -1 ) { data.numElements * data.byteStride } else { limit * data.byteStride } //convert the numElements realm limit into byte limit
//
//			if ( effectiveLimit > 0 ) {
//				tmpBuffer.position(0)
//				tmpBuffer.limit(effectiveLimit)
//
//				if ( data.name == 0 ){
//					data.name = glGenBuffers()
//				}
//				VBO.bindBuffer(bufferType,data.name)
//
////				if ( effectiveLimit / data.byteStride == data.numSolidifiedElements ) { //if the size hasn't changed
////					glBufferSubData(bufferType,0,tmpBuffer)
////				} else {
//					glBufferData(bufferType,tmpBuffer,usage)
////				}
//			}
//
//			data.numSolidifiedElements = effectiveLimit / data.byteStride //account for the possibility of a supplied limit
////		}
//	}
//
//	def unsolidify () {
//		lcl_subUnsolidify(points)
//		lcl_subUnsolidify(indices)
//		lastSolidifiedMarker = 0
//		solidifiedLevelOfDetail = -1
//	}
//
//	def lcl_subUnsolidify(data: GLData) {
////		data synchronized {
//			if ( data.name != 0 ){
//	////			glDeleteBuffers(data.name)
//	//			VBO.buffersToDelete synchronized {
//	//				VBO.buffersToDelete ::= data.name
//	//				//Previously buffersToDelete contained GLData objects, and the name was zeroed out on deletion
//	//				data.name = 0
//	//			}
//	////			data.name = 0
//				GL.vbosToDestroy synchronized {
//					GL.vbosToDestroy.enqueue(data.name)
//					data.numSolidifiedElements = 0
//					data.name = 0
//				}
//			}
////		}
//	}
//
//	def isSolidified = points.name != 0 || indices.name != 0
//
//	def numPoints: Int = points.numElements
//	def numPoints_= (n: Int){
////		points.synchronized{
//			points.resizeElements(n)
////		}
//	}
//	def numIndices: Int = indices.numElements
//	def numIndices_= (n: Int){
////		indices.synchronized{
//			indices.resizeElements(n)
////		}
//	}
//	def numPointsHint (n: Int){
////		points.synchronized{
//			points.reserveElements(n)
////		}
//	}
//	def numIndicesHint (n: Int){
////		indices.synchronized{
//			indices.reserveElements(n)
////		}
//	}
//
//	def getC (n: Int): Vec4f = points.getAsVec4(n,subArrayInfo(VBO.ColorArray).offset)
//	def getV (n: Int): Vec3f = points.getAsVec3(n,subArrayInfo(VBO.VertexArray).offset)
//	def getN (n: Int): Vec3f = points.getAsVec3(n,subArrayInfo(VBO.NormalArray).offset)
//	def getT (n: Int): Vec2f = points.getAsVec2(n,subArrayInfo(VBO.TexCoordArray).offset)
//
//	def getI (n: Int): Int = indices.getAsInt(n)
//
//	def setC (n: Int, v: ReadVec4f){ points.set(n,subArrayInfo(VBO.ColorArray).offset,v) }
//	def setV (n: Int, v: ReadVec3f){ points.set(n,subArrayInfo(VBO.VertexArray).offset,v) }
//	def setN (n: Int, v: ReadVec3f){ points.set(n,subArrayInfo(VBO.NormalArray).offset,v) }
//	def setT (n: Int, v: ReadVec2f){ points.set(n,subArrayInfo(VBO.TexCoordArray).offset,v) }
//
//	def setC (n: Int, r: Float,g: Float, b: Float, a: Float){ points.set(n,subArrayInfo(VBO.ColorArray).offset,r,g,b,a) }
//	def setC (n: Int, rgba: Int, v: Float){ points.set(n,subArrayInfo(VBO.ColorArray).offset,rgba,v) }
//	def setV (n: Int, x: Float,y: Float,z: Float){ points.set(n,subArrayInfo(VBO.VertexArray).offset,x,y,z) }
//	def setN (n: Int, x: Float,y: Float,z: Float){ points.set(n,subArrayInfo(VBO.NormalArray).offset,x,y,z) }
//	def setT (n: Int, x: Float,y: Float){ points.set(n,subArrayInfo(VBO.TexCoordArray).offset,x,y) }
//
//	def setI (n: Int, i: Short) {
//		if ( indices.dataType == GL_UNSIGNED_SHORT ) {
//			indices.set(n,i)
//		} else {
//			indices.set(n,i.toInt)
//		}
//	}
//	def setI (n: Int, i: Int) {
//		if ( indices.dataType == GL_UNSIGNED_SHORT ) {
//			if ( i > (Short.MaxValue << 1) ) { throw new IllegalStateException("Index out of range in VBO : " + i) }
//			indices.set(n,i.toShort)
//		} else {
//			indices.set(n,i)
//		}
//	}
//
//	def isEmpty = (! isSolidified && numPoints == 0 && numIndices == 0) || (isSolidified && points.numSolidifiedElements == 0 && indices.numSolidifiedElements == 0)
//	def nonEmpty = ! isEmpty
//
//	def useIntIndices () {
//		indices = new GLData(GL_UNSIGNED_INT)
//		indices.intStride = 1
//	}
//}