package arx.core

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/12/12
 * Time: 9:40 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._


import arx.application.{Noto}
import arx.core.introspection.ReflectionAssistant

trait TDependable {
	var _dependencies : List[ Class[_] ] = List()
	def dependencies : List[ Class[_] ] = _dependencies ; def dependencies_= ( s : List[ Class[_]] ) { _dependencies = s }
	var resolvedDependencies : List[AnyRef] = Nil
	def reify[T <: AnyRef : Manifest] : T = {
		resolvedDependencies.find( d => manifest[T].erasure.isAssignableFrom(d.getClass) ) match {
			case Some(t) => t.asInstanceOf[T]
			case None => {
				if ( dependencies.exists( c => manifest[T].erasure.isAssignableFrom(c) ) ) {
	//				Noto.warn("Dependency resolution failed, attempted matches for " + manifest[T].erasure.getSimpleName + ":")
	//				resolvedDependencies.foreach( d => Noto.warn("\t" + d.getClass.getSimpleName + ": " + manifest[T].erasure.isAssignableFrom(d.getClass)) )
	//				Noto.warn("Printall: " + resolvedDependencies)
	//				throw new IllegalStateException("Attempting to get dependency before dependencies have been resolved")
					resolvedDependencies.find( d => manifest[T].erasure.isAssignableFrom(d.getClass) ) match {
						case Some(t) => t.asInstanceOf[T]
						case None => if ( dependencies.exists( c => manifest[T].erasure.isAssignableFrom(c) ) ) {
							Noto.warn("Dependency resolution failed, attempted matches for " + manifest[T].erasure.getSimpleName + ":")
							resolvedDependencies.foreach( d => if ( manifest[T].erasure.isAssignableFrom(d.getClass)) { return d.asInstanceOf[T] } )
							throw new IllegalStateException("Attempting to get dependency before dependencies have been resolved")
						} else {
							throw new IllegalStateException("Attempting to get instance of class not specified as a dependency")
						}
					}
				} else {
					throw new IllegalStateException("Attempting to get instance of class not specified as a dependency")
				}
			}
		}
	}

	def subDependables : List [ TDependable ] = Nil
}

object Dependency {
	def recursiveResolveDependables ( l : List[TDependable] ) : List[TDependable] = {
		l match {
			case Nil => Nil
			case someList =>
				val newDependables = someList.flatMap( _.subDependables )
				l ::: newDependables ::: recursiveResolveDependables(newDependables)
		}
	}

	def resolve ( dependables_a : List[TDependable] , resolveAgainst : List[AnyRef], instantiationFunc : (List[Class[_]]) => AnyRef = (l) => ReflectionAssistant.firstInstanceOf(l)) : List[AnyRef] = {
		var dependables = recursiveResolveDependables(dependables_a)
		var res = resolveAgainst
		while ( dependables.nonEmpty ) {
			val generator = dependables.head
			for ( dep <- generator.dependencies ) {
				generator.resolvedDependencies ::= (res.find( g => dep.isAssignableFrom(g.getClass) ) match {
					case Some(g) => g
					case None =>
						val instOpt =
						if ( dep.isInterface || java.lang.reflect.Modifier.isAbstract(dep.getModifiers) ){
							instantiationFunc(ReflectionAssistant.allSubTypesOf(dep))
						} else {
							try{
								Some(dep.newInstance())
							} catch {
								case e : Exception => None
							}
						}

						instOpt match {
							case Some(inst) =>
								inst match {
									case newGenerator : TDependable =>
										res ::= newGenerator
										dependables :+= newGenerator
										newGenerator
									case o : AnyRef =>
										res ::= o
										o
									case _ => throw new IllegalStateException("something other than AnyRef encountered during dependency resolution");
								}
							case None => throw new IllegalStateException("Could not find acceptable dependency filler of type : " + dep);
						}
				})
			}
			if ( ! res.contains(generator) ) { res ::= generator }
			dependables = dependables.tail
		}
		res
	}

	def resolve ( dependables_a : List[TDependable] ) : List[AnyRef] = resolve(dependables_a,dependables_a)
	def resolve ( dependable : TDependable , resolveAgainst : List[AnyRef] ) : List[AnyRef] = resolve(List(dependable),resolveAgainst)

	def topologicalSort[T <: TDependable] ( dependables : List[T] ) : List[T] = {
		var res = List[T]()
		var set = Set[T]()
		def add ( d : T ) {
			if ( ! set.contains(d) ) {
				set += d
				d.resolvedDependencies foreach { case subDependency : T => add(subDependency) ; case _ => }
				res ::= d
			}
		}
		dependables foreach { add }
		res = res.reverse.filter { e => dependables.contains(e) }
		res
	}
}


class DepA extends TDependable{
	dependencies = List(classOf[DataC])
}
class DepB extends TDependable{
	dependencies = List(classOf[DepA],classOf[DataC])
}
class DepC extends TDependable{
	dependencies = List(classOf[DepA],classOf[DepB])
}
class DataC {}

class CircDepA extends TDependable {
	dependencies = List(classOf[CircDepB])
}
class CircDepB extends TDependable {
	dependencies = List(classOf[CircDepA])
}

