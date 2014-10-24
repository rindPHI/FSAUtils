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
	import Conversions._
	import FSAMethods._
	import Helpers._
	import RegularExpressions._
	import Relations._
	
	import Predef.{any2stringadd => _, _}
  
	class DFA(
	    var alphabet: Set[Letter],
	    var states: Set[State],
	    var initialState: State,
	    var delta: ((State, Letter) => State),
	    var accepting: Set[State]) {
	  
	  require(states contains initialState)
	  require(accepting subsetOf states)
	  
	  def accepts(word: String): Boolean =
	    accepts(for (x <- word.toList) yield Symbol(x toString))
	  
	  def accepts(word: Word): Boolean = accepts(word, initialState)
	  
	  def accepts(word: Word, fromState: State): Boolean = word match {
	    case Nil => accepting contains fromState
	    case letter :: rest => accepts(rest, delta (fromState, letter))
	  }
	
	  def unary_! : DFA = new DFA(alphabet, states, initialState, delta, states -- accepting)
	  
	  def * : NFA = (this: NFA)*
	  
	  def ++(other: NFA): DFA = 
	    ((this: NFA) ++ other) toDFA
	    
	  def ++(other: DFA): DFA = 
	    this ++ (other: NFA)
	  
	  private def productAutomaton(other: DFA) : DFA = {
	    val productStates = cartesianStateProduct(states, other.states)

	    def productDelta(s: State, l: Letter): State = s match {
	      case pair(s1,s2) => pair(delta(s1,l), other.delta(s2,l))
	      case _ => error("Impossible case")
	    }
	    
	    (alphabet, productStates, pair(initialState, other.initialState), productDelta _, Set(): Set[State])
	  }
	    
	  def &(other: DFA): DFA = {
	    val intersAccepting = cartesianStateProduct(accepting, other.accepting)
	    val product = productAutomaton(other)
	    
	    (alphabet, product.states, product.initialState, product.delta, intersAccepting)
	  }
	  
	  def |(other: DFA): DFA = {
	    val unionAccepting = cartesianStateProduct(accepting, other.states) ++
	    		             cartesianStateProduct(states, other.accepting)
	    val product = productAutomaton(other)
	    
	    (alphabet, product.states, product.initialState, product.delta, unionAccepting)
	  }
	  
	  def \(other: DFA): DFA =
	    this & (!other)
	  
	  def ==(other: DFA): Boolean =
	    ((this \ other) isEmpty) && ((other \ this) isEmpty)
	    
	  def isEmpty: Boolean = accepting.foldLeft(true)(
			  (acc, s) => acc && !traverseDFS(List(initialState), List()).contains(s)
	      )
	      
	  def toRegExp: RE = {
	    // Rename states: 1 .. n
	    val renDFA = this getRenamedCopy 1
	    renDFA.accepting.foldLeft(Empty(): RE)(
	        (re, s) => re + renDFA.alpha(renDFA.states.size, renDFA initialState, s)
	    )
	  }
	  
	  def getRenamedCopy(startVal: Int): DFA = {
	    val emptyMap : Map[State, State] = Map()
	    val renameMap : Map[State, State] =
	      states.foldLeft(emptyMap){ (z,s) =>
	      	z + (s -> q(z.size + startVal))
	      }
	    val reverseRenameMap = renameMap.map(_.swap)
	    
	    def deltaRen (state: State, letter: Letter) : State =
		  renameMap(delta(reverseRenameMap(state), letter))
	    
	    (alphabet,
	        states.map(s => renameMap(s)),
	        renameMap(initialState),
	        deltaRen _,
	        accepting.map(s => renameMap(s)))
	  }
	  
	  def minimize: DFA = {
	    // first, remove unreachable states
	    val reachableStates = states intersect (Set() ++ traverseDFS(List(initialState), List()))
	    val reachableAccepting = accepting intersect reachableStates
	    
	    val rel = (reachableStates -- reachableAccepting)
		    .foldLeft(new AntiReflSymmRel(): AntiReflSymmRel[State])(
		        (rel,s) => reachableAccepting.foldLeft(new AntiReflSymmRel(): AntiReflSymmRel[State])(
		        	(_,a) => rel + (s, a)
		        )
		    )
		    
		val reachDFA = (alphabet, reachableStates, initialState , delta, reachableAccepting) : DFA
	    
	    reachDFA minimize rel
	  }
	  
	  private def minimize(rel: AntiReflSymmRel[State]): DFA = {
	    val cartProd = cartesianProduct(states, states)
	    val differentPairs = cartProd.filter(p => p match {
	        case (k, l) => rel.inRel(k, l) ||
	        		alphabet.foldLeft(false)(
	        		    (acc,a) => acc || rel.inRel(delta(k,a), delta(l,a))
	        		)
	  	    case _ => error("Should not happen")
		})
	    
		val newRel = rel ++ differentPairs
	    
	    if (rel == newRel) {
	        
	        // Recursion anchor:
	        // The distinguishing relation does not change in the next
	        // iteration, so we construct the resulting automaton now
	        // by "contracting" the states that are not distinguished
	        
	        val eqRel: EquivRel[State] =
	            new EquivRel() ++ cartProd.filter(p => !rel.inRel(p._1,p._2))
	        
	        val newStates = eqRel.equivalenceClasses.map(
	            setOfStates => set(setOfStates)
	        ): Set[State]
	        
	        val newInitialState = newStates.filter(state => state match {
	            case set(setOfStates: Set[State]) => setOfStates contains initialState
	            case _ => error("Impossible case.")
	        }) head: State
	        
	        val newAccepting = newStates.filter(state => state match {
	            case set(setOfStates: Set[State]) => (setOfStates intersect accepting).nonEmpty
	            case _ => error("Impossible case.")
	        }): Set[State]

            def newDelta(state: State, letter: Letter): State =
                (state) match {
                    case set(setOfStates) => {
                        val someState = setOfStates head
                        val transResult = delta(someState, letter)
                        
                        newStates.filter(state => state match {
				            case set(setOfStates: Set[State]) => setOfStates contains transResult
				            case _ => error("Impossible case.")
				        }) head
                    }
                    case _ => error("Impossible case.")
                }
	        
	    	(alphabet, newStates, newInitialState, newDelta _, newAccepting) : DFA
	    	
	    } else {
	        
	    	minimize(newRel)
	    	
	    }
	  }
	  
	  private def alpha(k: Int, from: State, to: State): RE =
	    if (k == 1) {
	      val oneStepTransitions = alphabet
	    		  .filter(a => delta(from, a) == to)
	    		  .foldLeft(Empty(): RE)((re,a) => re + a)
	      
	      if (from == to) {
	        (Empty()*) + oneStepTransitions
	      } else {
	        oneStepTransitions
	      }
	    } else {
	      (from,to) match {
	        case (q(l),q(m)) =>
	          alpha(k-1, q(l), q(m)) +
	          (alpha(k-1, q(l), q(k)) &
	              (alpha(k-1, q(k),q(k))*) &
	              alpha(k-1, q(k), q(m)))
	        
	        case _ => error("Should not happen: Call toRegExp() and not this method")
	      }
	    }
	    
	  private def traverseDFS(toVisit: List[State], visited: List[State]): List[State] = {
	    if (toVisit isEmpty) {
	      List()
	    } else {
	      val next = toVisit head
	      val succ = alphabet.map(l => delta(next, l)).toList diff toVisit diff visited
	      
	      next :: traverseDFS(toVisit.tail ++ succ, next :: visited)
	    }
	  }
	  
	  override def toString = {
	    val indentSpace = "    "
	    val indentBeginner = "|"
	    val indent = "|" + indentSpace
	    val dindent = indent + indentSpace
	    var sb = new StringBuilder()
	    
	    sb ++= "DFA (Z,S,q0,d,A) with\n"
	    
	    sb ++= toStringUpToDelta(
	        indentBeginner,
	        indentSpace,
	        "Z", alphabet,
	        "S", states,
	        "q0", initialState,
	        "A", accepting);
	      
	    sb ++= indent ++= "d = {"
	    states.foreach(s =>
	      alphabet.foreach(l =>
	        sb ++= "\n"
	           ++= dindent
	           ++= "("
	             ++= s.toString
	             ++= ","
	             ++= l.name
	             ++= ") => "
	             ++= delta(s,l).toString
	           ++= ","))
	    sb = sb.dropRight(1 - alphabet.isEmpty)
	    sb ++= "\n" ++= indent ++= "}\n"
	      
	    sb toString
	  }
	}
}