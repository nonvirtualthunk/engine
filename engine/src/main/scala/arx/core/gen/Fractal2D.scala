package arx.core.gen

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 12/24/11
 * Time: 12:46 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.core.gen.SimplexNoise

import arx.core.vec.{ReadVec2f, ReadVec3f}

class Fractal2D ( src : GenSource2D, val iterations : Int , val scalar : Float = 2.1384f ) extends GenSource2D {
	def apply ( tx : Float , ty : Float ) : Float = {
		var sum = 0.0f
		var multiplier = 1.0f
		var x = tx
		var y = ty
		var i = iterations
		while ( i > 0 ){
			sum += src.apply(x,y) * multiplier
			x *= scalar
			y *= scalar
			multiplier /= scalar
			i -= 1
		}
		sum
	}
}

class Fractal3D ( src : GenSource3D, val iterations : Int , val scalar : Float = 2.1384f ) extends GenSource3D {
	def apply ( tx : Float , ty : Float , tz : Float ) : Float = {
		var sum = 0.0f
		var multiplier = 1.0f
		var x = tx
		var y = ty
		var z = tz
		var i = iterations
		while ( i > 0 ){
			sum += src.apply(x,y,z) * multiplier
			x *= scalar
			y *= scalar
			z *= scalar
			multiplier /= scalar
			i -= 1
		}
		sum
	}
}

class ScaledInputs2D ( src : GenSource2D , val scalarx : Float , val scalary : Float ) extends GenSource2D {
	def apply ( x : Float , y : Float ) : Float = {
		src.apply(x * scalarx,y * scalary)
	}
}
class ScaledInputs3D ( src : GenSource3D , val scalarx : Float , val scalary : Float , val scalarz : Float ) extends GenSource3D {
	def apply ( x : Float , y : Float , z : Float) : Float = {
		src.apply(x * scalarx,y * scalary,z * scalarz)
	}
}
class TranslatedInputs2D ( src : GenSource2D , val scalarx : Float , val scalary : Float ) extends GenSource2D {
	def apply ( x : Float , y : Float ) : Float = {
		src.apply(x + scalarx,y + scalary)
	}
}
class TranslatedInputs3D ( src : GenSource3D , val scalarx : Float , val scalary : Float , val scalarz : Float ) extends GenSource3D {
	def apply ( x : Float , y : Float , z : Float) : Float = {
		src.apply(x + scalarx,y + scalary,z + scalarz)
	}
}
class ScaleOutput2D ( src : GenSource2D , scale : Float ) extends GenSource2D {
	def apply ( x : Float ,y  : Float ) : Float = { src(x,y) * scale }
}
class ScaleOutput3D ( src : GenSource3D , scale : Float ) extends GenSource3D {
	def apply ( x : Float ,y  : Float , z : Float ) : Float = { src(x,y,z) * scale }
}
class AbsoluteValue2D ( src : GenSource2D ) extends GenSource2D {
	def apply(x: Float, y: Float) = math.abs(src.apply(x,y))
}
class AbsoluteValue3D ( src : GenSource3D ) extends GenSource3D {
	def apply(x: Float, y : Float , z : Float ) = math.abs(src.apply(x,y,z))
}

class SimplexSource2D ( src : SimplexNoise ) extends GenSource2D {
	def apply(x: Float, y: Float) : Float = src.noise(x,y)
}
class SimplexSource3D ( src : SimplexNoise ) extends GenSource3D {
	def apply(x: Float, y: Float, z: Float) : Float = src.noise(x,y,z)
}
object SimplexSource{
	implicit def noise2SimplexSource2D ( src : SimplexNoise ) : SimplexSource2D = new SimplexSource2D(src)
	implicit def noise2SimplexSource3D ( src : SimplexNoise ) : SimplexSource3D = new SimplexSource3D(src)
}

trait GenSource {
	def apply : Float
}
trait GenSource2D {
	def apply( x : Float , y : Float ) : Float
	def scaleInputs ( x : Float, y : Float ) : GenSource2D = new ScaledInputs2D(this,x,y)
	def translateInputs ( x : Float , y : Float ) : GenSource2D = new TranslatedInputs2D(this,x,y)
	def scaleOutput ( f : Float ) : GenSource2D = new ScaleOutput2D(this,f)
}
trait GenSource3D {
	def apply( x : Float , y : Float , z : Float ) : Float
	def scaleInputs ( x : Float, y : Float , z : Float ) : GenSource3D = new ScaledInputs3D(this,x,y,z)
	def translateInputs ( x : Float , y : Float , z : Float ) : GenSource3D = new TranslatedInputs3D(this,x,y,z)
	def scaleOutput ( f : Float ) : GenSource3D = new ScaleOutput3D(this,f)
}
class FunctionSource2D ( f : (Float,Float) => Float ) extends GenSource2D{
	def apply ( x : Float , y : Float ) : Float = { f(x,y) }
}
class FunctionSource3D ( f : (Float,Float,Float) => Float ) extends GenSource3D{
	def apply ( x : Float , y : Float , z : Float ) : Float = { f(x,y,z) }
}

object GenSource2D{
	implicit def function2GenSource2D ( f : (Float,Float) => Float ) : GenSource2D = {
		new FunctionSource2D(f)
	}
}
object GenSource3D{
	implicit def function2GenSource3D ( f : (Float,Float,Float) => Float ) : GenSource3D = {
		new FunctionSource3D(f)
	}
}




trait OutputTransformer extends ((Float) => Float) with Serializable {

}
trait InputTransformer extends Serializable {
	@inline def transformX ( x : Float ) : Float
	@inline def transformY ( y : Float ) : Float
	@inline def transformZ ( z : Float ) : Float

	@inline def transformX_XY ( x : Float , y : Float ) : Float = transformX(x)
	@inline def transformY_XY ( x : Float , y : Float ) : Float = transformY(y)

	@inline def transformX_XYZ ( x : Float , y : Float , z : Float ) : Float = transformX(x)
	@inline def transformY_XYZ ( x : Float , y : Float , z : Float ) : Float = transformY(y)
	@inline def transformZ_XYZ ( x : Float , y : Float , z : Float ) : Float = transformZ(z)

	def >> ( generator : Generator ) = new InputTransformedGenerator(this,generator)
	def >> ( transformer : InputTransformer ) = new ChainedInputTransformer(this,transformer)
}

class ChainedInputTransformer ( first : InputTransformer , second : InputTransformer ) extends InputTransformer {
	@inline def transformX ( x : Float ) : Float = second.transformX(first.transformX(x))
	@inline def transformY ( y : Float ) : Float = second.transformY(first.transformY(y))
	@inline def transformZ ( z : Float ) : Float = second.transformZ(first.transformZ(z))

	@inline override def transformX_XY ( x : Float , y : Float ) : Float = second.transformX_XY(first.transformX_XY(x,y),first.transformY_XY(x,y))
	@inline override def transformY_XY ( x : Float , y : Float ) : Float = second.transformY_XY(first.transformX_XY(x,y),first.transformY_XY(x,y))

	@inline override def transformX_XYZ ( x : Float , y : Float , z : Float ) : Float = second.transformX_XYZ(first.transformX_XYZ(x,y,z),first.transformY_XYZ(x,y,z),first.transformZ_XYZ(x,y,z))
	@inline override def transformY_XYZ ( x : Float , y : Float , z : Float ) : Float = second.transformY_XYZ(first.transformX_XYZ(x,y,z),first.transformY_XYZ(x,y,z),first.transformZ_XYZ(x,y,z))
	@inline override def transformZ_XYZ ( x : Float , y : Float , z : Float ) : Float = second.transformZ_XYZ(first.transformX_XYZ(x,y,z),first.transformY_XYZ(x,y,z),first.transformZ_XYZ(x,y,z))
}

trait Generator extends Serializable {
	def apply ( x : Float ) : Float
	def apply ( x : Float , y : Float ) : Float
	def apply ( x : Float , y : Float , z : Float ) : Float

	def apply ( xy : ReadVec2f ) : Float = apply(xy.x,xy.y)
	def apply ( xyz : ReadVec3f ) : Float = apply(xyz.x,xyz.y,xyz.z)

	def transformOutput ( transformer : OutputTransformer ) : Generator = new OutputTransformedGenerator(this,transformer)
	def transformInput ( transformer : InputTransformer ) : Generator = new InputTransformedGenerator(transformer,this)

	def >> ( transformer : (Float) => Float ) : Generator = new OutputTransformedGenerator(this,transformer)
//	def -> ( meta : MetaGenerator ) : Generator = {
//		meta.subGenerator = this
//		meta
//	}

}

trait MetaGenerator extends Generator {
	var subGenerator : Generator = null

	def apply ( generator : Generator ) = {
		subGenerator = generator
		this
	}
	def << ( generator : Generator ) = {
		subGenerator = generator
		this
	}
}


class InputTransformedGenerator(transformer : InputTransformer,generator : Generator) extends Generator {
	def apply(x : Float) = generator( transformer.transformX(x) )
	def apply(x: Float, y: Float) = generator( transformer.transformX_XY(x,y), transformer.transformY_XY(x,y) )
	def apply(x: Float, y: Float, z : Float) = generator( transformer.transformX_XYZ(x,y,z) ,transformer.transformY_XYZ(x,y,z), transformer.transformZ_XYZ(x,y,z) )
}
class OutputTransformedGenerator(generator : Generator,transformer : (Float) => Float) extends Generator {
	def apply(x: Float) = transformer(generator(x))
	def apply(x: Float, y: Float) = transformer(generator(x,y))
	def apply(x: Float, y: Float, z : Float) = transformer(generator(x,y,z))
}

object ArxGenerators {
	def Simplex ( simplex : SimplexNoise ) = new Simplex(simplex)
	class Simplex ( simplex : SimplexNoise ) extends Generator {
		def apply(x: Float) = simplex.noise(x)
		def apply(x: Float, y: Float) = simplex.noise(x,y)
		def apply(x: Float, y: Float, z: Float) = simplex.noise(x,y,z)
	}

	object Abs extends OutputTransformer {
		def apply(x: Float) = math.abs(x)
	}

	def Normalize(min:Float,max:Float) = new Normalize(min,max)
	class Normalize(min:Float,max:Float) extends OutputTransformer {
		val range = math.max(max - min,0.00001f)
		def apply ( x : Float ) = (x - min) / range
	}

	def Add ( addBy : Float ) = new Add(addBy)
	class Add(addBy : Float) extends OutputTransformer { def apply ( x : Float ) = x + addBy }

	def Mult(multBy: Float) = new Mult(multBy)
	class Mult(multBy: Float) extends OutputTransformer { def apply ( x : Float ) = x * multBy }

	def Min(min: Float) = new Min(min)
	class Min(min: Float) extends OutputTransformer { def apply ( x : Float ) = math.min(x,min) }

	def Max(max: Float) = new Max(max)
	class Max(max: Float) extends OutputTransformer { def apply ( x : Float ) = math.max(x,max) }

	def Clamp(min:Float,max:Float) = new OutputTransformer { def apply ( x : Float ) = clamp(x,min,max) }

	object Square extends OutputTransformer {
		def apply ( x : Float ) = x * x
	}
	def Pow (f:Float) = new Pow(f)
	class Pow(f:Float) extends OutputTransformer {
		def apply ( x : Float ) = powf(x,f)
	}

	def Translate ( x : Float , y : Float , z : Float ) = new Translate(ReadVec3f(x,y,z))
	def Translate ( offset : ReadVec3f ) = new Translate(offset)
	class Translate ( offset : ReadVec3f ) extends InputTransformer {
		def transformX(x: Float) = x + offset.x
		def transformY(y: Float) = y + offset.y
		def transformZ(z: Float) = z + offset.z
	}

	def Sum ( gen1 : Generator , gen2 : Generator ) = new Generator {
		def apply(x: Float, y: Float, z: Float): Float = gen1(x,y,z) + gen2(x,y,z)
		def apply(x: Float, y: Float): Float = gen1(x,y) + gen2(x,y)
		def apply(x: Float): Float = gen1(x) + gen2(x)
	}

	def Scale ( x : Float , y : Float , z : Float ) = new Scale(ReadVec3f(x,y,z))
	def Scale ( x : Float ) = new Scale(ReadVec3f(x,x,x))
	def Scale ( scale : ReadVec3f ) = new Scale(scale)
	class Scale ( scale : ReadVec3f ) extends InputTransformer {
		def transformX(x: Float) = x * scale.x
		def transformY(y: Float) = y * scale.y
		def transformZ(z: Float) = z * scale.z
	}

	object PrintGenerator extends Generator {
		def apply(x: Float) = { println("Input : " + x) ; x }
		def apply(x: Float, y: Float) = { println("Input : " + x + "," + y) ; x }
		def apply(x: Float, y: Float, z: Float) = { println("Input : " + x + "," + y + "," + z) ; x }
	}
	object PrintOutputTransformer extends OutputTransformer {
		def apply ( x : Float ) = { println("Output : " + x); x }
	}

	val IdentityGenerator = new Generator {
		def apply(x: Float): Float = 1.0f
		def apply(x: Float, y: Float): Float = 1.0f
		def apply(x: Float, y: Float, z: Float): Float = 1.0f
	}
	val ZeroGenerator = new Generator {
		def apply(x: Float): Float = 0.0f
		def apply(x: Float, y: Float): Float = 0.0f
		def apply(x: Float, y: Float, z: Float): Float = 0.0f
	}

	protected class Turbulence ( turbulenceGenerator : Generator ) extends InputTransformer {
		def transformX(x: Float): Float = x + turbulenceGenerator.apply(x)
		def transformY(y: Float): Float = y + turbulenceGenerator.apply(y)
		def transformZ(z: Float): Float = z + turbulenceGenerator.apply(z)

		override def transformX_XY(x: Float, y: Float): Float = x + turbulenceGenerator.apply(x,y)
		override def transformY_XY(x: Float, y: Float): Float = y + turbulenceGenerator.apply(x + 1000.0f,y + 1000.0f)

		override def transformX_XYZ(x: Float, y: Float, z: Float): Float = x + turbulenceGenerator.apply(x,y,z)
		override def transformY_XYZ(x: Float, y: Float, z: Float): Float = y + turbulenceGenerator.apply(x + 1000.0f,y + 1000.0f,z + 1000.0f)
		override def transformZ_XYZ(x: Float, y: Float, z: Float): Float = z + turbulenceGenerator.apply(x + 2000.0f,y + 2000.0f,z + 2000.0f)
	}
	def Turbulence( gen : Generator ) = new Turbulence(gen)


	def Fractal(iterations:Int,scalar:Float=2.1384f) = new FractalGenerator(iterations,scalar)
	class FractalGenerator(iterations:Int,scalar:Float=2.1384f) extends MetaGenerator {
		def apply ( tx : Float , ty : Float , tz : Float ) : Float = {
			var sum = 0.0f
			var multiplier = 1.0f
			var x = tx
			var y = ty
			var z = tz
			var i = iterations
			while ( i > 0 ){
				sum += subGenerator.apply(x,y,z) * multiplier
				x *= scalar
				y *= scalar
				z *= scalar
				multiplier /= scalar
				i -= 1
			}
			sum
		}
		def apply ( tx : Float , ty : Float ) : Float = {
			var sum = 0.0f
			var multiplier = 1.0f
			var x = tx
			var y = ty
			var i = iterations
			while ( i > 0 ){
				sum += subGenerator.apply(x,y) * multiplier
				x *= scalar
				y *= scalar
				multiplier /= scalar
				i -= 1
			}
			sum
		}
		def apply ( tx : Float ) : Float = {
			var sum = 0.0f
			var multiplier = 1.0f
			var x = tx
			var i = iterations
			while ( i > 0 ){
				sum += subGenerator.apply(x) * multiplier
				x *= scalar
				multiplier /= scalar
				i -= 1
			}
			sum
		}
	}
}
