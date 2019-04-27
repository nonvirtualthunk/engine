package arx.core.richer


class RicherTraversable[T](val intern : Traversable[T]) extends AnyVal {

	def sumOf[N : Numeric](f : T => N) : N = {
		intern.map(f).sum
	}
}
