package de.dominicscheurer.fsautils {
	import Types._
	import RegularExpressions._
	
	object Conversions {
	  implicit def bool2int(b:Boolean) = if (b) 1 else 0
	  
	  implicit def DFAFromTuple[T <: State](
	      t: (Set[Letter],
	          Set[T],
	          T,
	          ((T, Letter) => T),
	          Set[T])) : DFA[T] = {
	    new DFA[T](t._1, t._2, t._3, t._4, t._5)
	  }
	  
	  implicit def NFAFromTuple[T <: State](
	      t: (Set[Letter],
	          Set[T],
	          T,
	          ((T, Letter) => Option[Set[T]]),
	          Set[T])) : NFA[T] = {
	    new NFA(t._1, t._2, t._3, t._4, t._5)
	  }
	  
	  implicit def DFAtoNFA[T <: State](dfa: DFA[T]) : NFA[T] =
	    (dfa.alphabet, dfa.states, dfa.initialState,
	        (state: T, letter: Letter) => Some(Set(dfa.delta(state, letter))),
	        dfa.accepting)
	  
	  implicit def REFromLetter(
	      letter: Letter) : RE = {
	    L(letter)
	  }
	  
	  // The following could be dangerous for short
	  // regular expressions (conflicting implicits):
	  implicit def OptionSetFromStates[T <: State](
	      state: T) : Option[Set[T]] =
	    Some(Set(state))
	  
	  implicit def OptionSetFromStates[T <: State](
	      states: (T, T)) =
	    Some(Set(states._1, states._2))
	
	  implicit def OptionSetFromStates[T <: State](
	      states: (T, T, T)) =
	    Some(Set(states._1, states._2, states._3))
	}
}