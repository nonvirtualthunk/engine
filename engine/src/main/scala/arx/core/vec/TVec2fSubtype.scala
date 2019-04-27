package arx.core.vec

/*
This would probably need to be handled by adding stuff to ReadVec3i or one of its super classes instead, unfortunately
the types T/ReadVec3i are erased out so you can't do a distinct + only when they're both T's, you have to override all
+ Vec3i's. It works ok, but it's not ideal. As-is it "works" but requires an additional allocation for every operation
which is probably prohibitive for vectors
 */


//trait TVec2fSubtype[T <: Vec2f] extends Vec2f {
//	def create(v : ReadVec2f) : T
//
//	def +(v: T): T = create(super.+(v))
//	def -(v : T): T = create(super.-(v))
//}
//
trait TVec2iSubtype extends ReadVec2i {
	type T <: ReadVec2i

	def create(v : ReadVec2i) : T

	override def +(v : ReadVec2i) : T = create(super.+(v))
	override def -(v : ReadVec2i) : T = create(super.-(v))
	override def *(i : Int) : T = create(super.*(i))

}
trait TVec3iSubtype extends ReadVec3i {
	type T <: ReadVec3i
	
	def create(v : ReadVec3i) : T

	override def +(v : ReadVec3i) : T = create(super.+(v))
	override def -(v : ReadVec3i) : T = create(super.-(v))
	override def *(i : Int) : T = create(super.*(i))
	
}