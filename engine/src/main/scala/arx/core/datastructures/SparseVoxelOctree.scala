package arx.core.datastructures

import java.io.{Externalizable, ObjectInput, ObjectOutput}

import arx.application.Noto
import arx.core.query.{ContinuousQuery, ContinuousQueryListener, TContinuousQuerySource}
import arx.core.vec.Vec3i
import arx.core.vec.coordinates.{TMajorCoord, ObjectCoord, VoxelCoord}

import scala.collection.mutable

/**
 *
 */

class SparseVoxelOctree[T >: Null] ( granularity : Int = 32 ) {
	val root = new SparseVoxelOctree.OctNode
	val center = VoxelCoord.Center
	val baseIntervalBits = 11
	var minIntervalBits = math.round(math.log(granularity) / math.log(2)).toInt
	val baseInterval = 1 << baseIntervalBits

	def findNode ( x : Int, y : Int, z : Int , create : Boolean ) : SparseVoxelOctree.ValueNode[T] = {
		var nx = center.x
		var ny = center.y
		var nz = center.z
		var intervalBits = baseIntervalBits
		var node : SparseVoxelOctree.Node = root

		while ( intervalBits >= minIntervalBits ) {
			val dx = if (x < nx) { 0 } else { 1 }
			val dy = if (y < ny) { 0 } else { 1 }
			val dz = if (z < nz) { 0 } else { 1 }
			val childIndex = (dx << 2) | (dy << 1) | (dz)
			val onode = node.asInstanceOf[SparseVoxelOctree.OctNode]
			if ( onode.children(childIndex) == null ) {
				if ( create ) {
					if ( intervalBits == minIntervalBits ) {
						onode.children(childIndex) = new SparseVoxelOctree.ValueNode[T];
					} else {
						onode.children(childIndex) = new SparseVoxelOctree.OctNode
					}
				} else { return null }
			}
			node = onode.children(childIndex)
			val halfInterval = 1 << (intervalBits - 1)
			nx = nx - halfInterval + (dx << intervalBits)
			ny = ny - halfInterval + (dy << intervalBits)
			nz = nz - halfInterval + (dz << intervalBits)
			intervalBits -= 1
		}
		node.asInstanceOf[SparseVoxelOctree.ValueNode[T]]
	}
	def set ( p : Vec3i , v : T ) : SparseVoxelOctree.ValueNode[T]= {
		set(p.x,p.y,p.z,v)
	}
	def set ( p : ObjectCoord , v : T ) : SparseVoxelOctree.ValueNode[T]= {
		set(p.toVoxelCoord,v)
	}
	def remove ( p : ObjectCoord , toRemove : T ) { remove(p.toVoxelCoord,toRemove) }
	def remove ( p : VoxelCoord , toRemove : T ) { remove(p.x,p.y,p.z,toRemove) }
	def remove ( x : Int, y : Int, z : Int , toRemove : T ) { remove(x,y,z,toRemove,false) }
	def removeFromNode ( n : SparseVoxelOctree.ValueNode[T] , toRemove : T ) { n.values = n.values.filterNot(_ == toRemove) }

	def remove (t :TMajorCoord , toRemove : T , ignoreIfNotPresent : Boolean ) { val vc = t.toVoxelCoord; remove(vc.x,vc.y,vc.z,toRemove,ignoreIfNotPresent) }
	def remove (x : Int , y : Int , z : Int , toRemove : T , ignoreIfNotPresent : Boolean ) {
		var nx = center.x
		var ny = center.y
		var nz = center.z
		var intervalBits = baseIntervalBits
		var node : SparseVoxelOctree.Node = root

		while ( intervalBits >= minIntervalBits ) {
			val dx = if (x < nx) { 0 } else { 1 }
			val dy = if (y < ny) { 0 } else { 1 }
			val dz = if (z < nz) { 0 } else { 1 }
			val childIndex = (dx << 2) | (dy << 1) | (dz)
			val onode = node.asInstanceOf[SparseVoxelOctree.OctNode]
			if ( onode.children(childIndex) == null ) {
				if ( ! ignoreIfNotPresent ) { Noto.warn("Attempting to remove non-present node from SVO") }
				return
			}

			if ( intervalBits == minIntervalBits ) {
				val valueNode = onode.children(childIndex).asInstanceOf[SparseVoxelOctree.ValueNode[T]]
				valueNode.values = valueNode.values filterNot { v => toRemove == v }
			}
			else { node = onode.children(childIndex) }

			val halfInterval = 1 << (intervalBits - 1)
			nx = nx - halfInterval + (dx << intervalBits)
			ny = ny - halfInterval + (dy << intervalBits)
			nz = nz - halfInterval + (dz << intervalBits)
			intervalBits -= 1
		}
	}
	def set ( p : VoxelCoord , v : T ) : SparseVoxelOctree.ValueNode[T] = { set(p.x,p.y,p.z,v); }
	def set ( x : Int, y : Int, z : Int , v : T ) : SparseVoxelOctree.ValueNode[T] = { val node = findNode(x,y,z,true); node.values :+= v; node }
	def get ( p : ObjectCoord ) : List[T] = get(p.toVoxelCoord)
	def get ( p : VoxelCoord ) : List[T] = get(p.x,p.y,p.z)
	protected def get ( p : Vec3i ) : List[T] = { get(p.x,p.y,p.z) }
	/** Gets all values that are in the block that contains the given x,y,z coord */
	def get ( x : Int , y : Int , z : Int ) : List[T] = {
		val r = findNode(x,y,z,false)
		if ( r != null ) { r.values } else { Nil }
	}

                 //1 2 4 8 16 32 64 128
	val bdx = Array(0,0,0,0,1,1,1,1)
	val bdy = Array(0,0,1,1,0,0,1,1)
	val bdz = Array(0,1,0,1,0,1,0,1)
	def getInVolume ( o : ObjectCoord , r : Int ) : List[T] = getInVolume(o.toVoxelCoord,r)
	def getInVolume ( v : VoxelCoord , r : Int ) : List[T] = getInVolume(v.x,v.y,v.z,r)
	def getInVolume ( x : Int, y : Int , z : Int , r : Int ) : List[T] = {
		var intervalBits = baseIntervalBits
		var nodes : List[(SparseVoxelOctree.Node,Int,Int,Int)] = List((root,center.x,center.y,center.z))
		while ( intervalBits >= minIntervalBits ) {
			var nextNodes : List[(SparseVoxelOctree.Node,Int,Int,Int)] = Nil
			for ( ntup <- nodes ) {
				val halfInterval = 1 << (intervalBits - 1)
				val node = ntup._1
				val nx = ntup._2
				val ny = ntup._3
				val nz = ntup._4

				var childIndices = 0xff
				if ( x + r < nx ) { childIndices &= 0x0f }       //00001111
				else if ( x - r >= nx ) { childIndices &= 0xf0 } //11110000
				if ( y + r < ny ) { childIndices &= 0x33 }       //00110011
				else if ( y - r >= ny ) { childIndices &= 0xcc } //11001100
				if ( z + r < nz ) { childIndices &= 0x55 }       //01010101
				else if ( z - r >= nz ) { childIndices &= 0xaa } //10101010

				var i = 0;while ( i < 8 ) {
					if ( ( childIndices & (1 << i) ) != 0 ) {
						val n = node.asInstanceOf[SparseVoxelOctree.OctNode].children(i)
						if ( n != null ) {
							val tx = nx - halfInterval + (bdx(i) << intervalBits)
							val ty = ny - halfInterval + (bdy(i) << intervalBits)
							val tz = nz - halfInterval + (bdz(i) << intervalBits)
							nextNodes ::= (n,tx,ty,tz)
						}
					}
				i += 1}
			}
			intervalBits -= 1
			nodes = nextNodes
		}
		var resultList : List[T] = Nil
		val iter = nodes.iterator
		while ( iter.hasNext ) {
			resultList :::= iter.next()._1.asInstanceOf[SparseVoxelOctree.ValueNode[T]].values
		}
		resultList
//		nodes.flatMap { _._1.asInstanceOf[SparseVoxelOctree.ValueNode[T]].values }
	}

	class Iterator(node : SparseVoxelOctree.Node) {
		val nodeStack = new mutable.Stack[SparseVoxelOctree.Node]
		val countStack = new mutable.Stack[Int]
		nodeStack.push(node)
		countStack.push(0)

		def hasNext = { nodeStack.nonEmpty && nodeStack.top.isInstanceOf[SparseVoxelOctree.ValueNode[T]] }
		def next = {
			val ret = nodeStack.top.asInstanceOf[SparseVoxelOctree.ValueNode[T]].values
		}
	}
}

class QueryBackedSparseVoxelOctree[T >: Null <: AnyRef : Manifest](var posFunc : (T) => VoxelCoord,var query : ContinuousQuery[T],var resolution : Int = 32) extends
			SparseVoxelOctree[T](resolution) with ContinuousQueryListener[T] with Externalizable
{
	def this () { this(null,null,0) }

	if ( query != null ) {  //query _should_ only be null if we are reading from disk
		query.withListener(this,fireOnExistingResults = true)
	}
	def queryResultAdded(t: T) {
		set(posFunc(t),t)
	}
	def queryResultRemoved(t: T) {
		remove(posFunc(t),t)
	}

	def writeExternal(p1: ObjectOutput) {
		p1.writeObject(posFunc)
		p1.writeObject(query.matchFunction)
		p1.writeObject(query.source)
		p1.writeInt(resolution)
	}
	def readExternal(p1: ObjectInput) {
		posFunc = p1.readObject.asInstanceOf[(T)=>VoxelCoord]
		val matchFunc = p1.readObject.asInstanceOf[(AnyRef)=>Option[T]]
		val source = p1.readObject.asInstanceOf[Option[TContinuousQuerySource]]
		resolution = p1.readInt

		query = new ContinuousQuery[T](matchFunc).withListener(this,fireOnExistingResults = false)
		source match {
			case Some(src) => src.registerQuery(query)
			case None => Noto.error("Unbacked SVO encountered")
		}
	}
}

object SparseVoxelOctree {
	class Foo {
		var a = math.random
		var s = math.random.toString
	}


	abstract class Node
	class OctNode extends Node {
		val children = new Array[Node](8)
	}
	class ValueNode[T] extends Node {
		var values : List[T] = Nil
	}
}
