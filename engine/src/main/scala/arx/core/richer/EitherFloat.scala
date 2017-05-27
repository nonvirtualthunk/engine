package arx.core.richer

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/7/13
 * Time: 9:28 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._


import arx.application.Noto

class EitherFloat ( val a : Float , val b : Float ) {
	def aeq ( p : EitherFloat , eps : Float ) : Boolean = { p.a.aeq(a,eps) || p.b.aeq(b,eps) }
	def =~= ( p : EitherFloat ) : Boolean = this.aeq(p,0.00001f)
	def aeq ( f : Float , eps : Float ) : Boolean = { a.aeq(f,eps) || b.aeq(f,eps) }
	def =~= ( f : Float ) : Boolean = aeq(f,0.00001f)
	def + ( p : Float ) : EitherFloat = new EitherFloat(a + p,b + p)
	def - ( p : Float ) : EitherFloat = new EitherFloat(a - p,b - p)
	def * ( p : Float ) : EitherFloat = new EitherFloat(a * p,b * p)
}
