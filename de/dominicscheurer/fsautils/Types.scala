package de.dominicscheurer.fsautils

object Types {
	type Letter = Symbol
	type Word = List[Letter]
	type NFADeltaResult = Option[Set[State]]
	
	abstract sealed class State
	case class q(i: Int) extends State
	case class set(s: Set[Int])
	
	type States = Set[State]
}