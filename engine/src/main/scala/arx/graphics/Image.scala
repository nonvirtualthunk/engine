package arx.graphics

import java.awt.image.BufferedImage
import java.io._
import java.nio.ByteBuffer
import javax.imageio.ImageIO

import arx.application.Noto
import arx.core.gen.Generator
import arx.core.math.Recti
import arx.core.metrics.Metrics
import arx.core.traits.TSentinelable
import arx.core.vec.{ReadVec4i, Vec2i, Vec4f, Vec4i}
import org.newdawn.slick.opengl.PNGImageData2

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/30/11
 * Time: 4:13 PM
 * Created by nonvirtualthunk
 */

class Image extends TSentinelable with Externalizable {
	var hasAlpha: Boolean = true

	var width: Int = 0
	var height: Int = 0
	def aspectRatio = width.toFloat / math.max(height.toFloat,1.0f)
	def invAspectRatio = height.toFloat / math.max(width.toFloat,1.0f)
	var textureWidth: Int = 0
	var textureHeight: Int = 0
	var data: ByteBuffer = null
	var resourcePath : Option[String] = None
	var lastModified : Option[Long] = None
	var sentinel = false
	override def isSentinel = sentinel

	var revision = 0

	def dimensions = Vec2i(width,height)

	override def writeExternal(p1: ObjectOutput): Unit = {
		if (data != null) {
			p1.writeBoolean(true)
			p1.writeInt(width)
			p1.writeInt(height)
			p1.writeInt(textureWidth)
			p1.writeInt(textureHeight)
			p1.writeObject(resourcePath)
			p1.writeObject(lastModified)
			p1.writeBoolean(sentinel)
			p1.writeInt(revision)
			val tmp = Array.ofDim[Byte](width * height * 4)
			data.get(tmp,0,width * height * 4)
			p1.write(tmp)
		} else {
			p1.writeBoolean(false)
		}
	}

	override def readExternal(p1: ObjectInput): Unit = {
		if (p1.readBoolean) {
			width = p1.readInt
			height = p1.readInt
			textureWidth = p1.readInt
			textureHeight = p1.readInt
			resourcePath = p1.readObject().asInstanceOf[Option[String]]
			lastModified = p1.readObject().asInstanceOf[Option[Long]]
			sentinel = p1.readBoolean
			revision = p1.readInt
			val dataIn = Array.ofDim[Byte](width * height * 4)
			p1.read(dataIn)
			data = ByteBuffer.allocateDirect(width * height * 4)
			data.rewind()
			data.put(dataIn)
			data.rewind()
		}
	}

	def fileName = resourcePath match {
		case None => "UnknownSource"
		case Some(rp) => rp.split("/").toList match {
			case Nil => "Root"
			case other => other.last
		}
	}

	def raw (x : Int,y : Int, rgba : Int) : Byte = data.get(y * textureWidth * 4 + x * 4 + rgba)

	def apply ( x: Int, y: Int ) : Vec4i = {
		data.position(y * textureWidth * 4 + x * 4)
		Vec4i(data.get() & 0xff,data.get() & 0xff,data.get() & 0xff,data.get() & 0xff)
	}
	def apply ( x : Int , y : Int , rgba: Int ) : Int = {
		data.get(y * textureWidth * 4 + x * 4 + rgba) & 0xff
	}
	def update ( x: Int, y: Int , v : ReadVec4i) {
		data.position(y * textureWidth * 4 + x * 4)
		data.put(v.r.toByte)
		data.put(v.g.toByte)
		data.put(v.b.toByte)
		data.put(v.a.toByte)
	}

	def colorAtV4( x : Int, y : Int ) = {
		data.position(y * textureWidth * 4 + x * 4)
		Vec4f((data.get() & 0xff) / 255.0f,(data.get() & 0xff) / 255.0f,(data.get() & 0xff) / 255.0f,(data.get() & 0xff) / 255.0f)
	}

	def pixelIndex ( x : Int , y : Int ) = y * textureWidth * 4 + x * 4
	def setAtIndex ( i : Int , rgba : Int , b : Byte )  { data.put(i + rgba,b) }
	def update ( x : Int , y : Int , rgba: Int , b: Byte ){
		data.put(y * textureWidth * 4 + x * 4 + rgba,b)
	}


	def downsample(factor: Int) : Image = {
		var target: Image = null
		var f = 0
		var src = this

		while ( f < factor ){
			target = Image.withDimensions(textureWidth >> factor,textureHeight >> factor)
			for ( x <- 0 until textureWidth by 2 ; y <- 0 until textureHeight by 2 ) {
				target(x,y) = (src(x,y) + src(x+1,y) + src(x,y+1) + src(x+1,y+1)) / 4
			}
			src = target
			f += 1
		}

		target
	}

	/**
	 * Sets every pixel in the image to according to the function supplied and returns itself
	 * @param func function to determine the values at each (x,y,rgba), scale of 0-255
	 * @return self
	 */
	def setPixelsFromFunc ( func : (Int,Int,Int) => Int ) : Image = {
		var x = 0; while ( x < width ) {
			var y = 0; while ( y < height ) {
				var q = 0; while ( q < 4 ) {
					this(x,y,q) = func(x,y,q).toByte
				q += 1}
			y += 1}
		x += 1}
		this
	}

	/**
	 * Sets every pixel in the image to according to the function supplied and returns itself
	 * @param func function to determine the values at each (x,y), scale of 0-255
	 * @return self
	 */
	def setPixelsFromFunc ( func : (Int,Int) => ReadVec4i ) : Image = {
		var x = 0; while ( x < width ) {
			var y = 0; while ( y < height ) {
				this(x,y) = func(x,y)
			y += 1}
		x += 1}
		this
	}
	def setPixelsFromGenerator ( gen : Generator ) {
		var x = 0; while ( x < width ) {
			var y = 0; while ( y < height ) {
				val f = gen(x,y)
				val i = (arx.Prelude.clamp(f,0.0f,1.0f) * 255).toByte
				this(x,y,0) = i
				this(x,y,1) = i
				this(x,y,2) = i
				this(x,y,3) = 255.toByte
			y += 1}
		x += 1}
	}
	def transformPixelsByFunc ( func : (Int,Int,ReadVec4i) => ReadVec4i ) {
		val holder = Vec4i(0,0,0,0)
		var x = 0; while ( x < width ) {
			var y = 0; while ( y < height ) {
				var q = 0; while ( q < 4 ) {
					holder(q) = this(x,y,q)
				q += 1}
				val newVal = func(x,y,holder)
				if ( ! ( newVal eq holder ) ) {
					this(x,y) = newVal
				}
			y += 1}
		x += 1}
	}
	def foreachPixel[T] ( func : (Int,Int,ReadVec4i) => T ) {
		val holder = Vec4i(0,0,0,0)
		var x = 0; while ( x < width ) {
			var y = 0; while ( y < height ) {
				var q = 0; while ( q < 4 ) {
					holder(q) = this(x,y,q)
				q += 1}
				func(x,y,holder)
			y += 1}
		x += 1}
	}

	override def toString: String = {
		"Image(" + resourcePath.getOrElse("synthetic") + ")"
	}
}

class SubImageView(subImage:Image,region:Recti) extends Image {
	hasAlpha = subImage.hasAlpha
	width = region.width
	height = region.height
	data = subImage.data
	resourcePath = subImage.resourcePath
	lastModified = subImage.lastModified

	textureWidth = subImage.textureWidth
	textureHeight = subImage.textureHeight

	override def raw (x : Int,y : Int, rgba : Int) : Byte = {
		data.get((y + region.y) * textureWidth * 4 + (x + region.x) * 4 + rgba)
	}

	override def apply ( x: Int, y: Int ) : Vec4i = {
		data.position((y+region.y) * textureWidth * 4 + (x+region.x) * 4)
		Vec4i(data.get() & 0xff,data.get() & 0xff,data.get() & 0xff,data.get() & 0xff)
	}
	override def apply ( x : Int , y : Int , rgba: Int ) : Int = {
		data.get((y+region.y) * textureWidth * 4 + (x+region.x) * 4 + rgba) & 0xff
	}
	override def update ( x: Int, y: Int , v : ReadVec4i) {
		data.position((y+region.y) * textureWidth * 4 + (x+region.x) * 4)
		data.put(v.r.toByte)
		data.put(v.g.toByte)
		data.put(v.b.toByte)
		data.put(v.a.toByte)
	}
	override def pixelIndex ( x : Int , y : Int ) = (y+region.y) * textureWidth * 4 + (x+region.x) * 4
	override def setAtIndex ( i : Int , rgba : Int , b : Byte )  {
		throw new UnsupportedOperationException("No set by index in image sub view")
	}
	override def update ( x : Int , y : Int , rgba: Int , b: Byte ){
		data.put((y+region.y) * textureWidth * 4 + (x+region.x) * 4 + rgba,b)
	}
}

object Image{
	val timer = Metrics.timer("ImageLoad")

	def loadFromFile (filePath: String): Image = {
		val file = new File(filePath)
		val stream = new FileInputStream(file)
		val image = loadFromStream(stream,closeOnFinish = true)
		image.resourcePath = Some(filePath)
		image.lastModified = Some(file.lastModified)

		image
	}

	def loadFromStream (inputStream : InputStream,closeOnFinish : Boolean) : Image = {
		try {
			import arx.Prelude._
			timer.timeStmt {
				val ret = new Image

				val data = new PNGImageData2
				val buffer = data.loadImage(inputStream,true,true,null)
				if ( closeOnFinish ) { inputStream.close() }

				ret.data = buffer
				ret.width = data.getWidth
				ret.height = data.getHeight
				ret.hasAlpha = data.hasAlpha
				ret.textureWidth = data.getTexWidth
				ret.textureHeight = data.getTexHeight


				ret


//				val reader = new PngReader(inputStream)
//				val lineSet = reader.readRows()
//				val firstLine = lineSet.getImageLine(0).asInstanceOf[ImageLineInt]
//				val height = firstLine.imgInfo.rows
//				val width = firstLine.imgInfo.cols
//
//
//				ret.data = BufferUtils.createByteBuffer(height * width * 4)
//				ret.data.rewind()
//
//				for (rowI <- 0 until lineSet.size() optimized) {
//					val rowOffset = (lineSet.size - 1 - rowI) * width * 4
//					val line = lineSet.getImageLine(rowI).asInstanceOf[ImageLineInt]
//					for (j <- 0 until line.getSize optimized) {
//						line.getScanline
//						ret.data.put(rowOffset + j, line.getElem(j).toByte)
//					}
//
//				}
//
//				ret.width = width
//				ret.height = height
//				ret.hasAlpha = firstLine.imgInfo.alpha
//				ret.textureWidth = width
//				ret.textureHeight = height

				ret
			}
		} catch {
			case io : Exception =>
				Noto.warn("Could not load from stream " + inputStream + ", reason :" + io)
				Image.Sentinel
		}
	}

	def withDimensions(width: Int,height: Int): Image = {
		val img = new Image
		img.width = width
		img.height = height
		img.textureWidth = width
		img.textureHeight = height
		img.data = ByteBuffer.allocateDirect(width * height * 4)
		img.lastModified = Some(System.currentTimeMillis())
		img
	}

	def withDimensions(width: Int,height: Int,baseColor : ReadVec4i): Image = {
		val img = new Image
		img.width = width
		img.height = height
		img.textureWidth = width
		img.textureHeight = height
		img.data = ByteBuffer.allocateDirect(width * height * 4)
		img.lastModified = Some(System.currentTimeMillis())

		var x = 0; while ( x < img.width ) {
			var y = 0; while ( y < img.height ) {
				var q = 0; while ( q < 4 ) {
					img(x,y,q) = baseColor(q).toByte
				q += 1}
			y += 1}
		x += 1}

		img
	}

	def saveBytes (image : Image) = {
		val bos = new ByteArrayOutputStream()
		Image.save(image,bos,autoClose = false)
		bos.close()

		bos.toByteArray
	}
	def save( image : Image , path : String ) { save(image,new File(path)) }
	def save( image : Image , file : File ) { save(image,new FileOutputStream(file),true)}
	def save( image : Image , stream : OutputStream , autoClose : Boolean ) {
		val icon = image
		val bi = new BufferedImage(icon.width,icon.height,BufferedImage.TYPE_INT_ARGB)

		val wr = bi.getData.createCompatibleWritableRaster()

		val packer = Array(0,0,0,0)
		for ( x <- 0 until icon.width ; y <- 0 until icon.height ) {
			val r = icon(x,y,Red).toInt & 0xff
			val g = icon(x,y,Green).toInt & 0xff
			val b = icon(x,y,Blue).toInt & 0xff
			val a = icon(x,y,Alpha).toInt & 0xff
			packer(0) = r
			packer(1) = g
			packer(2) = b
			packer(3) = a

			wr.setPixel(x,icon.height - y - 1,packer)
		}

		bi.setData(wr)
		val fos = stream
		ImageIO.write(bi,"png",fos)
		if (autoClose) {
			fos.close()
		}
	}

	def composite ( lowerImage : Image, upperImage : Image ) = {
		if ( lowerImage.width != upperImage.width || lowerImage.height != upperImage.height ) { Noto.warn("Composite image was intended to be used with images of the same size") }
		val newImage = Image.withDimensions(lowerImage.width.max(upperImage.width),lowerImage.height.max(upperImage.height))
		lowerImage.data.rewind()
		newImage.data.put(lowerImage.data)
		lowerImage.data.rewind()
		newImage.data.rewind()

		var x = 0; while ( x < upperImage.width ) {
			var y = 0; while ( y < upperImage.height ) {
				if ( (upperImage.raw(x,y,Alpha) & 0xff) > 0 ) {
					newImage(x,y,Red) = upperImage.raw(x,y,Red)
					newImage(x,y,Green) = upperImage.raw(x,y,Green)
					newImage(x,y,Blue) = upperImage.raw(x,y,Blue)
					newImage(x,y,Alpha) = upperImage.raw(x,y,Alpha)
				}
			y += 1}
		x += 1}

		newImage
	}

	def copy ( img : Image ) = {
		val newImage = Image.withDimensions(img.width,img.height)
		img.data.rewind()
		newImage.data.put(img.data)
		img.data.rewind()
		newImage.data.rewind()

		newImage
	}


	val Red = 0
	val Green = 1
	val Blue = 2
	val Alpha = 3

	val Sentinel = Image.withDimensions(1,1)
	Sentinel.sentinel = true
}

