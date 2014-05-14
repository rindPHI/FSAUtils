package de.dominicscheurer.fsautils

object Types {
	type Letter = Symbol
	type State = Symbol
	type Word = List[Letter]
	type NFADeltaResult = Option[Set[Letter]]
}