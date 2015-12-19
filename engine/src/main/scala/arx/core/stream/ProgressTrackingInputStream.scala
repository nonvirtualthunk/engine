package arx.core.stream

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 12/1/12
 * Time: 12:13 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import java.io.{InputStream, FilterInputStream}

class ProgressTrackingInputStream(in:InputStream) extends FilterInputStream(in) {
	var bytesRead = 0L

	override def read() = {
		updateBytesRead(1)
		super.read()
	}

	override def read(b: Array[Byte]) = {
		updateBytesRead(super.read(b)).toInt
	}

	override def read(b: Array[Byte], off: Int, len: Int) = {
		updateBytesRead(super.read(b, off, len)).toInt
	}

	override def skip(n: Long) = {
		updateBytesRead(super.skip(n))
	}

	override def mark(readLimit: Int) {
		throw new UnsupportedOperationException
	}

	override def markSupported() = false

	def updateBytesRead (bytes : Long ) = {
		if ( bytes > 0 ) {
			bytesRead += bytes
		}
		bytes
	}
}