package arx.core.math

import org.scalatest.FlatSpec

class FixedFractionalTest extends FlatSpec {

	"Fixed fractional arithmetic" should "give answers equivalent to what the real number arithmetic would" in {
		assert(Sext(6) / Sext(2) == Sext(3))
		assert(Sext(2) * Sext(3) == Sext(6))
		assert(Sext(1) + Sext(2) == Sext(3))
		assert(Sext(3) - Sext(2) == Sext(1))
		assert(Sext(6) / Sext(-3) == Sext(-2))
		assert(Sext(2) * Sext(-3) == Sext(-6))
	}

}
