/* Copyright 2014 Dominic Scheurer
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
    
	class FSA_DSL {
		case class FSABuilder(
		      t: (Symbol, Symbol, Symbol, Symbol, Symbol), isDFA: Boolean) {
		    // The elements that need to be filled
		    var alphabet : Option[Set[Letter]]                           = None
			var states   : Option[States]                                = None
			var q0       : Option[State]                                 = None
			var deltaF   : Option[(State, Letter) => State]              = None
			var deltaR   : Option[(State, Letter) => Option[Set[State]]] = None
			var A        : Option[States]                                = None
		    
			// Connectors for definition: "where" and "and"
		    def where = (input: (Symbol, Any)) => (input._2 match {
		        case symbols: SymbolSet => input._1 match {
		            case t._1 => { alphabet = Some(symbols.set); this }
		        }
		        case stateSet: IntSet => input._1 match {
		            case t._2 => { states = Some(stateSet.set.map(s => q(s))); this }
		            case t._5 => { A = Some(stateSet.set.map(s => q(s)));      this }
		        }
		        case state: Int => input._1 match {
		            case t._3 => { q0 = Some(q(state)); this }
		        }
		        case func: DeltaFun => input._1 match {
		            case t._4 => {
		                def myDelta (s: State, l: Letter): State = s match {
		                    case q(i) =>
		                        if (func.fun contains(i, l))
		                        	q(func.fun(i, l))
		                        else
		                            error("DFA transition function must be total, "
		                                + "but is not defined for (" + i.toString + "," + l.toString + ")")
		                    case _    => error("Should not occur")
		                }
		                deltaF = Some(myDelta)
		                this
		            }
		        }
		        case func: DeltaRel => input._1 match {
		            case t._4 => {
		                def myDelta (s: State, l: Letter): Option[Set[State]] = s match {
		                    case q(i) =>
		                        if (func.fun contains(i, l))
		                        	Some(func.fun(i, l).map(s => q(s)))
		                        else
		                            None
		                    case _    => error("Should not occur")
		                }
		                deltaR = Some(myDelta)
		                this
		            }
		        }
		    })
		    
		    def and = where
		    
		    def testDFADone : Boolean =
		        alphabet.isDefined &&
		        states.isDefined   &&
		        q0.isDefined       &&
		        deltaF.isDefined   &&
		        A.isDefined
		    
		    def testNFADone : Boolean =
		        alphabet.isDefined &&
		        states.isDefined   &&
		        q0.isDefined       &&
		        deltaR.isDefined   &&
		        A.isDefined
		    
		    def | : DFA =
		        if (testDFADone)
		        	new DFA(alphabet.get, states.get, q0.get, deltaF.get, A.get)
		        else
		            error("Some values of the DFA are still undefined")
		            
		    def || : NFA =
		        if (testNFADone)
		        	new NFA(alphabet.get, states.get, q0.get, deltaR.get, A.get)
		        else
		            error("Some values of the NFA are still undefined")
		}
		
		// Starting point: dfa/nfa function
		def dfa(t: (Symbol, Symbol, Symbol, Symbol, Symbol)) =
		    FSABuilder(t, true)
		def nfa(t: (Symbol, Symbol, Symbol, Symbol, Symbol)) =
		    FSABuilder(t, false)
		    
		// Syntactic Sugar
		object Delta {
			def apply (t: ((Int, Symbol), Int)*)      = DeltaFun(Map() ++ t)
			def apply (t: ((Int, Symbol), Set[Int])*) = DeltaRel(Map() ++ t)
		}
		    
		case class SymbolWrapper(s: Symbol) {
		    def ==>(vals: SymbolSet) = (s, vals)
		    def ==>(vals: IntSet) = (s, vals)
		    def ==>(aval: Int) = (s, aval)
		    def ==>(afun: DeltaFun) = (s, afun)
		    def ==>(afun: DeltaRel) = (s, afun)
		}
		    
		implicit def SymbToSymbWrapper(s: Symbol) = SymbolWrapper(s)
		
		case class IntSet(set: Set[Int])
		case class StringSet(set: Set[String])
		case class SymbolSet(set: Set[Symbol])
		case class DeltaFun(fun: Map[(Int, Letter), Int])
		case class DeltaRel(fun: Map[(Int, Letter), Set[Int]])
		implicit def is(set: Set[Int]) = IntSet(set)
		implicit def sts(set: Set[String]) = StringSet(set)
		implicit def sys(set: Set[Symbol]) = SymbolSet(set)
//		implicit def dfun(fun: Map[(Int, Letter), Int]) = DeltaFun(fun)
	}
}