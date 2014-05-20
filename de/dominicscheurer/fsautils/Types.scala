package de.dominicscheurer.fsautils {
	import Conversions._
  
	object Types {
		type Letter = Symbol
		type Word = List[Letter]
		type NFADeltaResult = Option[Set[State]]
		
		abstract sealed class State
		
		case class q(i: Int) extends State {
		  override def toString = i toString
		}
		
		case class set[T <: State](s: Set[T]) extends State {
		  override def toString = {
		    var sb = new StringBuilder
		    
		    sb ++= s.foldLeft("{")((result, state) => result + state.toString + ",")
		    sb = sb.dropRight(1 - s.isEmpty)
		    sb ++= "}"
		      
		    sb toString
		  }
		}
		
		case class pair(s1: State, s2: State) extends State
		
		type States = Set[State]
	}
}