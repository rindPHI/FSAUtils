package de.dominicscheurer.fsautils

import Types._

object Relations {
	class AntiReflSymmRel[A] private (val values: Set[(A,A)]) {
	    
	    def this() = this(Set(): Set[(A,A)])
	    
	    def +(a: A, b: A): AntiReflSymmRel[A] = {
		  require(a != b)
		  
		  new AntiReflSymmRel[A] (values + (a -> b) + (b -> a))
		}
	    
	    def ++(set: Set[(A,A)]): AntiReflSymmRel[A] =
	        set.foldLeft(this)((rel,p) => rel + (p._1,p._2))
		
		def inRel(a: A, b: A): Boolean =
		  values.contains(a -> b)
		 
		def ==(other: AntiReflSymmRel[A]) =
		  values.equals(other.values)
		  
		def !=(other: AntiReflSymmRel[A]) =
		  !values.equals(other.values)
		  
	}
	
	class EquivRel[A] private (val map: Set[(A,A)], val values: Set[A]) {
	    
	    def this() = this(Set(), Set())
	    
	    def +(a: A, b: A): EquivRel[A] = {		  
		  val newVals = map + (a -> b) + (b -> a)
		  val transClosure = transitiveClosure(newVals)
		  new EquivRel[A] (transClosure, getValues(transClosure))
		}
	    
	    def ++(set: Set[(A,A)]): EquivRel[A] =
	        set.foldLeft(this)((rel,p) => rel + (p._1,p._2))
		
		def inRel(a: A, b: A): Boolean =
		  map.contains(a -> b)
		 
		def ==(other: EquivRel[A]) =
		  map.equals(other.map)
		  
		def !=(other: EquivRel[A]) =
		  !map.equals(other.map)
		  
		def equivalenceClasses: Set[Set[A]] = {
	        val valList = values toList : List[A]
	        valList match {
	            case h :: t =>
	                equivalenceClasses(h, t)
	            case Nil =>
	                Set()
	        }
	    }
		  
		private def equivalenceClasses(forElem: A, rest: List[A]): Set[Set[A]] = {
	        val classForElem = rest
	        		.filter(p => inRel(forElem, p))
	        		.foldLeft(Set(): Set[A])((set,p) => set + p) + forElem
	        
	        println("Class for elem " + forElem + ": " + classForElem)
	        
	        rest match {
	            case h :: t =>
	                equivalenceClasses(h, t) + classForElem
	            case Nil =>
	                Set(classForElem)
	        }
	        
	    }
		  
		private def getValues(map: Set[(A,A)]): Set[A] =
		    map.foldLeft(Set(): Set[A])((acc,p) =>
		        acc + p._1 + p._2
		    )

	    private def addTransitive[A, B](s: Set[(A, B)]) = {
	        s ++ (for ((x1, y1) <- s; (x2, y2) <- s if y1 == x2) yield (x1, y2))
	    }
	
	    private def transitiveClosure[A, B](s: Set[(A, B)]): Set[(A, B)] = {
	        val t = addTransitive(s)
	        if (t.size == s.size) s else transitiveClosure(t)
	    }
	    
		override def toString(): String =
		    map toString
		  
	}
}