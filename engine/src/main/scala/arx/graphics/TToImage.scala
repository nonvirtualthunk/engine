package arx.graphics

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 6/15/12
 * Time: 8:53 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.{Application, Noto}
import arx.core.async.Executor
import arx.resource.ResourceManager
import java.util.concurrent.Executors
import arx.core.Moddable

trait TToImage {
	def image : Image

	override def equals(other : Any) = other match {
		case img : Image => img == image
		case timg : TToImage => timg.image == image
		case _ => false
	}
}
class WrappedImage(val img:Image) extends TToImage {
	def image = img

	override def toString: String = "WrappedImage(" + img + ")"
}
class FetchImage(val str:String) extends TToImage {
	def image = ResourceManager.getImage(str)

	override def toString: String = s"FetchImage($str)"

	override def equals(other: Any): Boolean = other match {
		case fimage : FetchImage => fimage.str == this.str
		case _ => false
	}

	override def hashCode(): Int = str.hashCode
}
class ImageFunc(func: => Image) extends TToImage {
	lazy val img = func
	def image = img
}

class LazyImageFunc(func: => Image) extends TToImage with Moddable[Image] {
	var img : Image = null
	def image = {
		if ( img eq null ) {
			img = LazyImageFunc.loadingImage
			LazyImageFunc.load(func,this)
			img
		} else { img }
	}

	def resolve() = image
	def baseValue() = image
	override def dynamic = true
}

class AsyncImage(func : => Image) extends LazyImageFunc(func) {
	private val tmp = image
}

object LazyImageFunc {
	val loadingImage = ResourceManager.getImage("ui/loading.png")


	def load (func : => Image,holder : LazyImageFunc) {
		Executor.submitAsync(new ImageLoaderRunnable(func,holder))//execute(new ImageLoaderRunnable(func,holder))
	}

	class ImageLoaderRunnable(func : => Image,holder : LazyImageFunc) extends Runnable {
		def run() {
			val tmp : Image = func
			holder.img = tmp
		}
	}



}
object TToImage {
	implicit def wrap (img:Image) : TToImage = new WrappedImage(img)
	implicit def wrap (str:String) : TToImage = new FetchImage(str)
	implicit def unwrap (t : TToImage) : Image = t.image

	def apply(img:Image) : TToImage = new WrappedImage(img)
	def apply (str:String) : TToImage = new FetchImage(str)
}