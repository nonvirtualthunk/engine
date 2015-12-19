package arx.graphics.traits

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 7/8/12
 * Time: 9:15 AM
 * Created by nonvirtualthunk
 */

import arx.graphics.AVBO

trait TRenderTarget {
	def vbo : AVBO
	def incrementVertexOffset ( n : Int ) : Int
	def incrementIndexOffset ( n : Int ) : Int
	def vertexOffset : Int
	def indexOffset : Int
}