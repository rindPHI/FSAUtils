/*
 * Copyright 2014 Dominic Scheurer
 * 
 * This file is part of FSAUtils.
 * 
 * FSAUtils is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * FSAUtils is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *  
 * You should have received a copy of the GNU General Public License
 * along with FSAUtils. If not, see <http://www.gnu.org/licenses/>.
 */

package de.dominicscheurer.fsautils {
	import Types._
	import RegularExpressions._
	
	object Conversions {
	  implicit def bool2int(b:Boolean) = if (b) 1 else 0
	  
	  implicit def DFAFromTuple(
	      t: (Set[Letter],
	          Set[State],
	          State,
	          ((State, Letter) => State),
	          Set[State])) : DFA = {
	    new DFA(t._1, t._2, t._3, t._4, t._5)
	  }
	  
	  implicit def NFAFromTuple(
	      t: (Set[Letter],
	          Set[State],
	          State,
	          ((State, Letter) => Option[Set[State]]),
	          Set[State])) : NFA = {
	    new NFA(t._1, t._2, t._3, t._4, t._5)
	  }
	  
	  implicit def DFAtoNFA[T](dfa: DFA) : NFA =
	    (dfa.alphabet, dfa.states, dfa.initialState,
	        (state: State, letter: Letter) => {
	            try {
	            	Some(Set(dfa.delta(state, letter)))
	            } catch {
	                case _: Throwable => None
	            }
	        },
	        dfa.accepting)
	  
	  implicit def REFromLetter(
	      letter: Letter) : RE = {
	    L(letter)
	  }
	  
	  // The following could be dangerous for short
	  // regular expressions (conflicting implicits):
	  implicit def OptionSetFromStates(
	      state: State) : Option[Set[State]] =
	    Some(Set(state))
	  
	  implicit def OptionSetFromStates(
	      states: (State, State)) =
	    Some(Set(states._1, states._2))
	
	  implicit def OptionSetFromStates(
	      states: (State, State, State)) =
	    Some(Set(states._1, states._2, states._3))
	}
}