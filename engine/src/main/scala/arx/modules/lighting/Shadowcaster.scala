package arx.modules.lighting

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 8/21/13
 * Time: 8:31 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.datastructures.RingBuffer
import arx.core.vec.Vec3f

import scalaxy.loops.rangeExtensions

object Shadowcaster {
	protected val ringBufferCache = new ThreadLocal[RingBuffer[Int]] {
		override def initialValue() = new RingBuffer[Int]
	}
	protected val relightBufferCache = new ThreadLocal[RingBuffer[Int]] {
		override def initialValue() = new RingBuffer[Int]
	}

	protected val ObstructionShadowLevel = 0.9f

	protected def provideRingBuffer = {
		val r = ringBufferCache.get
		r.clear()
		r
	}

	protected def provideRelightBuffer = {
		val r = relightBufferCache.get
		r.clear()
		r
	}

	/**
	 * Our standardized, modular shadowcasting implementation. Approximately simulates rays
	 * emanating from an origin of (0,0,0) outward in all directions. Obstructions will cast
	 * shadows outward from their starting point. Should be quite fast. <strong>All</strong>
	 * provided functions that take coordinate arguments are given <strong>relative</strong>
	 * coordinates. What precisely these mean are up to you, but can generally be thought of
	 * as voxels away from your origin point (light source, etc).
	 * @param obstructionFunction should return to what extent the given relative coordinate
	 *                            is obstructed by something (terrain, object, etc), return
	 *                            values should be in the range [0.0,1.0] with 0.0 indicating
	 *                            no obstruction, and 1.0 indicating total obstruction
	 * @param resultFunction the means by which results are passed back to the calling function,
	 *                       is provided the relative coordinate and pcnt visible (
	 *                       <strong>not</strong> pcnt shadowed) in the range [-1.0,1.0].
	 *                       Negative values indicate that an obstruction is present at
	 *                       this point, with a value of -1.0 indicating total obstruction,
	 *                       a value of -0.5 indicating half obstruction, etc.
	 * @param shadowGrid the grid used to hold the temporary shadow values, can be read from
	 *                   afterwards if desired, should be clear when provided
	 * @param lightStrength the "strength" of the theoretical "light" at your origin, determines
	 *                      the maximum distance that any ray may travel
	 * @param attenuation the falloff of the "light" irrespective of obstructions, takes a
	 *                    single argument equal to <code>currentDistance/lightStrength</code>
	 *                    and returns a value between [0.0,1.0] where 0.0 results in total
	 *                    shadow, regardless of other factors, and 1.0 would leave the normal
	 *                    shadow value unmodified.
	 * @param limitFunction returns true if a given relative coordinate is within the bounds
	 *                      of examination, false otherwise. Can be used to limit bounding
	 *                      from the default sphere limitation from <code>lightStrength</code>
	 *                      if desired. Defaults to always return true if not provided.
	 */
	def shadowcast (
		obstructionFunction : (Int,Int,Int) => Float,
		resultFunction : (Int,Int,Int,Float) => Unit,
		shadowGrid : TShadowGrid,
		lightStrength : Float,
		attenuation : (Float) => Float,
		limitFunction : (Int,Int,Int) => Boolean = (dx,dy,dz) => true
	) = {
		val Q = provideRingBuffer
		Q enqueue 0
		Q enqueue 0
		Q enqueue 0

		val startTime = System.nanoTime

		shadowGrid match {
			case osg : OctantShadowGrid => osg.clearTo(-1.toByte)
			case _ => Noto.warn("shadow grid provided to shadowcaster was not of type OctantShadowGrid, could not clear")
		}

		while ( Q.nonEmpty ) {
			//Coordinates relative to the light's position
			val dx = Q.dequeue()
			val dy = Q.dequeue()
			val dz = Q.dequeue()

			if ( limitFunction(dx,dy,dz) ) {
				val distance = sqrtf(dx*dx+dy*dy+dz*dz)
				if ( distance < lightStrength ) {
					if ( dx == 0 && dy == 0 && dz == 0 ) {
						shadowGrid(dx,dy,dz) = 0.0f
						resultFunction(dx,dy,dz,1.0f)
					} //do nothing, origin
					else {
						val obstructed = obstructionFunction(dx,dy,dz)
						if ( obstructed >= 0.95f ) {
							shadowGrid(dx,dy,dz) = ObstructionShadowLevel
							resultFunction(dx,dy,dz,-1.0f)
						} else {
							val absDx = math.abs(dx)
							val absDy = math.abs(dy)
							val absDz = math.abs(dz)

							val invAbsSum = 1.0f / (absDx + absDy + absDz).toFloat
							val pcntX = absDx.toFloat * invAbsSum
							val pcntY = absDy.toFloat * invAbsSum
							val pcntZ = absDz.toFloat * invAbsSum

							val signX = Integer.signum(dx)
							val signY = Integer.signum(dy)
							val signZ = Integer.signum(dz)

//							val shadowPcnt = if ( pcntX > pcntY && pcntX > pcntZ ) { shadowGrid(dx-signX,dy,dz) }
//							else if ( pcntY > pcntX && pcntY > pcntZ ) { shadowGrid(dx,dy-signY,dz) }
//							else { shadowGrid(dx,dy,dz-signZ) }
							val xContribution = shadowGrid(dx-signX,dy,dz) * pcntX
							val yContribution = shadowGrid(dx,dy-signY,dz) * pcntY
							val zContribution = shadowGrid(dx,dy,dz-signZ) * pcntZ

							if ( xContribution < 0 || yContribution < 0 || zContribution < 0 ) {
								Noto.warn(f"Checking against uninitialized shadowgrid $dx $dy $dz")
							}

							val shadowPcnt = (xContribution + yContribution + zContribution) * 1.015f
							val obstructionModifier = ObstructionShadowLevel * obstructed
							val effectiveShadowPcnt = math.min(shadowPcnt + obstructionModifier,ObstructionShadowLevel)

							shadowGrid(dx,dy,dz) = effectiveShadowPcnt
							val attenuatedLight = 1.0f - attenuation(distance / lightStrength)
							val newLight = attenuatedLight * (1.0f - effectiveShadowPcnt)

							resultFunction(dx,dy,dz,newLight)
						}
					}

					addSuccessors(Q,dx,dy,dz)
				}
			} else {
				shadowGrid(dx,dy,dz) = 1.0f
			}
		}

		val endtime = System.nanoTime
		Noto.finest("Time taken for shadowcast : " + ((endtime - startTime).toDouble / 1000000000.0) + "s")
	}

		protected def addSuccessors ( Q : RingBuffer[Int] , dx : Int , dy : Int , dz : Int ) {
			if ( dx == 0 && dy == 0 && dz == 0 ) { addV(Q,dx+1,dy,dz) ; addV(Q,dx,dy+1,dz) ; addV(Q,dx-1,dy,dz) ; addV(Q,dx,dy-1,dz) ; addV(Q,dx,dy,dz+1) ; addV(Q,dx,dy,dz-1) }
			else if ( dx == 0 && dy == 0 && dz < 0 ) { addV(Q,dx+1,dy,dz) ; addV(Q,dx,dy+1,dz) ; addV(Q,dx-1,dy,dz) ; addV(Q,dx,dy-1,dz) ; addV(Q,dx,dy,dz-1) }
			else if ( dx == 0 && dy == 0 && dz > 0 ) { addV(Q,dx+1,dy,dz) ; addV(Q,dx,dy+1,dz) ; addV(Q,dx-1,dy,dz) ; addV(Q,dx,dy-1,dz) ; addV(Q,dx,dy,dz+1) }
			else if ( dx > 0 && dy >= 0 ) { addV(Q,dx,dy+1,dz) }
			else if ( dx <= 0 && dy > 0 ) { addV(Q,dx-1,dy,dz) }
			else if ( dx < 0 && dy <= 0 ) { addV(Q,dx,dy-1,dz) }
			else if ( dx >= 0 && dy < 0 ) { addV(Q,dx+1,dy,dz) }

			if ( dx == 0 && dy > 0 ) { addV(Q,dx,dy+1,dz) }
			else if ( dx == 0 && dy < 0 ) { addV(Q,dx,dy-1,dz) }
			else if ( dx > 0 && dy == 0 ) { addV(Q,dx+1,dy,dz) }
			else if ( dx < 0 && dy == 0 ) { addV(Q,dx-1,dy,dz) }
		}

		protected def addV ( Q : RingBuffer[Int] , x : Int , y : Int , z : Int ) {
			Q.enqueue(x)
			Q.enqueue(y)
			Q.enqueue(z)
		}










	/**
	  * Our standardized, modular 8-way shadowcasting implementation. Approximately simulates rays
	  * emanating from an origin of (0,0,0) outward in all directions. Obstructions will cast
	  * shadows outward from their starting point. Should be quite fast. <strong>All</strong>
	  * provided functions that take coordinate arguments are given <strong>relative</strong>
	  * coordinates. What precisely these mean are up to you, but can generally be thought of
	  * as voxels away from your origin point (light source, etc).
	  * @param obstructionFunction should return to what extent the given relative coordinate
	  *                            is obstructed by something (terrain, object, etc), return
	  *                            values should be in the range [0.0,1.0] with 0.0 indicating
	  *                            no obstruction, and 1.0 indicating total obstruction
	  * @param resultFunction the means by which results are passed back to the calling function,
	  *                       is provided the relative coordinate and pcnt visible (
	  *                       <strong>not</strong> pcnt shadowed) in the range [-1.0,1.0].
	  *                       Negative values indicate that an obstruction is present at
	  *                       this point, with a value of -1.0 indicating total obstruction,
	  *                       a value of -0.5 indicating half obstruction, etc.
	  * @param shadowGrid the grid used to hold the temporary shadow values, can be read from
	  *                   afterwards if desired, should be clear when provided
	  * @param lightStrength the "strength" of the theoretical "light" at your origin, determines
	  *                      the maximum distance that any ray may travel
	  * @param attenuation the falloff of the "light" irrespective of obstructions, takes a
	  *                    single argument equal to <code>currentDistance/lightStrength</code>
	  *                    and returns a value between [0.0,1.0] where 0.0 results in total
	  *                    shadow, regardless of other factors, and 1.0 would leave the normal
	  *                    shadow value unmodified.
	  * @param limitFunction returns true if a given relative coordinate is within the bounds
	  *                      of examination, false otherwise. Can be used to limit bounding
	  *                      from the default sphere limitation from <code>lightStrength</code>
	  *                      if desired. Defaults to always return true if not provided.
	  */
	def shadowcast8 (
							obstructionFunction : (Int,Int,Int) => Float,
							resultFunction : (Int,Int,Int,Float) => Unit,
							shadowGrid : TShadowGrid,
							lightStrength : Float,
							attenuation : (Float) => Float,
							limitFunction : (Int,Int,Int) => Boolean = (dx,dy,dz) => true
						) = {
		val Q = provideRingBuffer
		Q enqueue 0
		Q enqueue 0
		Q enqueue 0

		val startTime = System.nanoTime

		shadowGrid match {
			case osg : OctantShadowGrid => osg.clearTo(-1.toByte)
			case _ => Noto.warn("shadow grid provided to shadowcaster was not of type OctantShadowGrid, could not clear")
		}

		while ( Q.nonEmpty ) {
			//Coordinates relative to the light's position
			val dx = Q.dequeue()
			val dy = Q.dequeue()
			val dz = Q.dequeue()

			if ( limitFunction(dx,dy,dz) ) {
				val distance = sqrtf(dx*dx+dy*dy+dz*dz)
				if ( distance < lightStrength ) {
					if ( dx == 0 && dy == 0 && dz == 0 ) {
						shadowGrid(dx,dy,dz) = 0.0f
						resultFunction(dx,dy,dz,1.0f)
					} //do nothing, origin
					else {
						val obstructed = obstructionFunction(dx,dy,dz)
						if ( obstructed >= 0.95f ) {
							shadowGrid(dx,dy,dz) = ObstructionShadowLevel
							resultFunction(dx,dy,dz,-1.0f)
						} else {
							val absDx = math.abs(dx)
							val absDy = math.abs(dy)
							val absDz = math.abs(dz)

							val invAbsSum = 1.0f / (absDx + absDy + absDz).toFloat
							val pcntX = absDx.toFloat * invAbsSum
							val pcntY = absDy.toFloat * invAbsSum
							val pcntZ = absDz.toFloat * invAbsSum

							val signX = Integer.signum(dx)
							val signY = Integer.signum(dy)
							val signZ = Integer.signum(dz)

							// ok,


//							var xContribution = 0.0f
//							var yContribution = 0.0f
//							var zContribution = 0.0f
//
//							// so if one factor dominates
//							if (pcntX > 0.6f || pcntY > 0.6f || pcntZ > 0.6f) {
//								xContribution = shadowGrid(dx-signX,dy,dz) * pcntX
//								yContribution = shadowGrid(dx,dy-signY,dz) * pcntY
//								zContribution = shadowGrid(dx,dy,dz-signZ) * pcntZ
//							}
//
//							if ( xContribution < 0 || yContribution < 0 || zContribution < 0 ) {
//								Noto.warn(f"Checking against uninitialized shadowgrid $dx $dy $dz")
//							}
//
//							val shadowPcnt = (xContribution + yContribution + zContribution) * 1.015f

							var shadowPcnt = 0.0f
							var maxDot = 0.0f

							val baseVector = Vec3f(dx,dy,dz)
							val normBaseVector = baseVector.normalize
							for (ddx <- 0 to 1 optimized; ddy <- 0 to 1 optimized; ddz <- 0 to 1 optimized) {
								if (ddx * signX != 0 || ddy * signY != 0 || ddz * signZ != 0) {
									val v = Vec3f(dx - ddx * signX, dy - ddy * signY, dz - ddz * signZ)
									if (v.x == 0.0f && v.y == 0.0f && v.z == 0.0f) {
										shadowPcnt = 0.0f
										maxDot = 0.0f
									} else {
										val normV = v.normalize
										val dot = normV.dot(normBaseVector)
										if (dot > maxDot) {
											val shadow = shadowGrid(dx - ddx * signX, dy - ddy * signY, dz - ddz * signZ)
											if (shadow < 0) {
												if (dx.abs < 5 && dy.abs < 5 )
												Noto.info(s"Checking against uninitialized $v, from $dx, $dy, $dz")
											} else {
												shadowPcnt = shadow
												maxDot = dot
											}
										}
									}
								}
							}
//
//							if (dotSum > 0.0f) {
//								shadowPcnt /= dotSum
//							}


							val obstructionModifier = ObstructionShadowLevel * obstructed
							val effectiveShadowPcnt = math.min(shadowPcnt + obstructionModifier,ObstructionShadowLevel)

							shadowGrid(dx,dy,dz) = effectiveShadowPcnt
							val attenuatedLight = 1.0f - attenuation(distance / lightStrength)
							val newLight = attenuatedLight * (1.0f - effectiveShadowPcnt)

							resultFunction(dx,dy,dz,newLight)
						}
					}

					addSuccessors(Q,dx,dy,dz)
				}
			} else {
				shadowGrid(dx,dy,dz) = 1.0f
			}
		}

		val endtime = System.nanoTime
		Noto.finest("Time taken for shadowcast : " + ((endtime - startTime).toDouble / 1000000000.0) + "s")
	}

	protected def addSuccessors8 ( Q : RingBuffer[Int] , dx : Int , dy : Int , dz : Int ) {
		if ( dx == 0 && dy == 0 && dz == 0 ) { addV(Q,dx+1,dy,dz) ; addV(Q,dx,dy+1,dz) ; addV(Q,dx-1,dy,dz) ; addV(Q,dx,dy-1,dz) ; addV(Q,dx,dy,dz+1) ; addV(Q,dx,dy,dz-1) }
		else if ( dx == 0 && dy == 0 && dz < 0 ) { addV(Q,dx+1,dy,dz) ; addV(Q,dx,dy+1,dz) ; addV(Q,dx-1,dy,dz) ; addV(Q,dx,dy-1,dz) ; addV(Q,dx,dy,dz-1) }
		else if ( dx == 0 && dy == 0 && dz > 0 ) { addV(Q,dx+1,dy,dz) ; addV(Q,dx,dy+1,dz) ; addV(Q,dx-1,dy,dz) ; addV(Q,dx,dy-1,dz) ; addV(Q,dx,dy,dz+1) }
		else if ( dx > 0 && dy >= 0 ) { addV(Q,dx,dy+1,dz) }
		else if ( dx <= 0 && dy > 0 ) { addV(Q,dx-1,dy,dz) }
		else if ( dx < 0 && dy <= 0 ) { addV(Q,dx,dy-1,dz) }
		else if ( dx >= 0 && dy < 0 ) { addV(Q,dx+1,dy,dz) }

		if ( dx == 0 && dy > 0 ) { addV(Q,dx,dy+1,dz) }
		else if ( dx == 0 && dy < 0 ) { addV(Q,dx,dy-1,dz) }
		else if ( dx > 0 && dy == 0 ) { addV(Q,dx+1,dy,dz) }
		else if ( dx < 0 && dy == 0 ) { addV(Q,dx-1,dy,dz) }
	}
}
