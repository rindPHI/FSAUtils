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
	import de.dominicscheurer.fsautils._
  
	import Types._
	import Conversions._
	import RegularExpressions._
	
	import Predef.{any2stringadd => _, _}
  
	object Main extends FSA_DSL {  
	  def main(args: Array[String]) {
	    
	    // DFA
	    // Non-DSL DFA creation:
	    def alphabet = Set('a, 'b)
		def states = Set(q(0), q(1))             : States
		def q0 = q(0)                            : State
//		def delta (state: State, letter: Letter) =
//		  (state, letter) match {
//			  case (q(0), 'a) => q(0)
//			  case (q(0), 'b) => q(1)
//			  case (q(1), 'a) => q(0)
//			  case (q(1), 'b) => q(1)
//			}
//		def A = Set(q(0))                        : States
//		
//		val myDFA = (alphabet, states, q0, delta _, A) : DFA
		
	    // DSL DFA creation:
		val myDFA =
		    dfa ('Z, 'S, 'q0, 'd, 'A) where
			    'Z  ==> Set('a, 'b)   and
			    'S  ==> Set(0, 1)     and
			    'q0 ==> 0             and
			    'A  ==> Set(0)        and
			    'd  ==> Delta(
					  (0, 'a) -> 0,
					  (0, 'b) -> 1,
					  (1, 'a) -> 0,
					  (1, 'b) -> 1
				)|
		
		print("DFA accepts aaab: ")
		println(myDFA accepts "aaab")
		print("DFA accepts aaaba: ")
		println(myDFA accepts "aaaba")
	    
		// Conversion
		val myDFAtoNFA = myDFA : NFA
		print("DFA2NFA accepts aaab: ")
		println(myDFAtoNFA accepts "aaab")
		print("DFA2NFA accepts aaaba: ")
		println(myDFAtoNFA accepts "aaaba")
		
	    // NFA
		def deltaNFA (state: State, letter: Letter) : NFADeltaResult =
		  (state, letter) match {
			  case (q(0), 'a) => (q(0), q(1))
			  case (q(0), 'b) => q(0)
			  case _         => None
			}
		def A_NFA = Set(q(1))                       : States
//		
//		val myNFA = (alphabet, states, q0, deltaNFA _, A_NFA) : NFA
		
		val myNFA =
			    nfa ('Z, 'S, 'q0, 'd, 'A) where
			        'Z  ==> Set('a, 'b)   and
			        'S  ==> Set(0, 1)     and
			        'q0 ==> 0             and
			        'A  ==> Set(1)        and
			        'd  ==> Delta(
			              (0, 'a) -> Set(0, 1),
			              (0, 'b) -> Set(0)
			        )||
		
		print("NFA accepts aaab: ")
		println(myNFA accepts "aaab")
		print("NFA accepts aaaba: ")
		println(myNFA accepts "aaaba")
		
		// DFA Operations
		print("!DFA accepts aaab: ")
		println((!myDFA) accepts "aaab")
		print("!DFA accepts aaaba: ")
		println((!myDFA) accepts "aaaba")
		
	    // NFA Operations
		def delta_NFAOp (state: State, letter: Letter) : NFADeltaResult =
		  (state, letter) match {
			  case (q(0), 'a) => q(0)
			  case (q(0), 'b) => q(1)
			  case _         => None
			}
		
		val myNFA2 = (alphabet, states, q0, delta_NFAOp _, A_NFA) : NFA
		
		print("NFA2 accepts aaab: ")
		println(myNFA2 accepts "aaab")
		print("NFA2 accepts aaabaaab: ")
		println(myNFA2 accepts "aaabaaab")
		
		print("(NFA2 toDFA) accepts aaab: ")
		println((myNFA2 toDFA) accepts "aaab")
		print("(NFA2 toDFA) accepts aaabaaab: ")
		println((myNFA2 toDFA) accepts "aaabaaab")
		
		print("!(NFA2 toDFA) accepts aaab: ")
		println(!(myNFA2 toDFA) accepts "aaab")
		print("!(NFA2 toDFA) accepts aaabaaab: ")
		println(!(myNFA2 toDFA) accepts "aaabaaab")
		
		print("NFA2* accepts aaab: ")
		println((myNFA2*) accepts "aaab")
		print("NFA2* accepts aaabaaab: ")
		println((myNFA2*) accepts "aaabaaab")
		
		println("\nNFA: ")
		println(myNFA)
		
		println("\n(NFA toDFA): ")
		println(myNFA toDFA)
		
		println("\nNFA2: ")
		println(myNFA2)
		
		println("\n(NFA2 toDFA): ")
		println(myNFA2 toDFA)
		
		// Concatenation
		println("\n(NFA ++ NFA2): ")
		println(myNFA ++ myNFA2)
				
		println("\n((NFA with epsilon) ++ NFA2): ")
		println((alphabet, states, q0, deltaNFA _, A_NFA ++ Set(q(0))) ++ myNFA2)
		
		// Intersection
		println("\n(DFA & DFA): ")
		println(myDFA & myDFA)
		
		// Difference
		println("\n(DFA \\ DFA): ")
		println(myDFA \ myDFA)
		
		// Emptyness
		println("\n(DFA isEmpty): ")
		println(myDFA isEmpty)
		
		println("\n((DFA \\ DFA) isEmpty): ")
		println((myDFA \ myDFA) isEmpty)
		
		// Equality
		println("\n(DFA == DFA): ")
		println(myDFA == myDFA)
		
		println("\n(DFA == NFA.toDFA): ")
		println(myDFA == myNFA.toDFA)
		
		// RegExp
		def myRegExp = (('a*) + ('b & ('b*) & 'a))* : RE
		println("\nRegExp:")
		println(myRegExp)
		
		println("\nDFA toRegExp:")
		println(myDFA toRegExp)
		println("\nDFA toRegExp (cleanString):")
		println((myDFA toRegExp) cleanString)
		println("\nNFA toRegExp (cleanString):")
		println((myNFA toRegExp) cleanString)
		
		// Minimization
		val detRenNFA = myNFA.toDFA.getRenamedCopy(0)
		println("\nNFA (renamed) toDFA:")
		println(detRenNFA)
		
		println("\nNFA (renamed) toDFA (minimized):")
		println(detRenNFA.minimize)
		
		println("\nNFA toDFA toRegExp (cleanString):")
		println(myNFA.toRegExp.cleanString)
		
		println("\nNFA toDFA (minimized) toRegExp (cleanString):") // Why is this not shorter???
		println(detRenNFA.toRegExp.cleanString)
		
		print("\nNFA toDFA (minimized) == NFA toDFA: ")
		println(myNFA.toDFA.minimize == myNFA.toDFA)
		
		// RE2NFS
		
		val re: RE = ('a*) + ('b)
//		val re: RE = ('b)
		println("Alphabet of " + (re cleanString) + ": " + re.alphabet)
		println(re.toNFA.toDFA.minimize.getRenamedCopy(0)) // Something does not work here!
		
//		println("\nDFA toRegExp:")
//		println(myDFA.toRegExp cleanString)
//		
//		println("\nDFA toRegExp toNFA:")
//		println(myDFA.toRegExp.toNFA)
//		
//		println("\nDFA toRegExp toNFA == DFA:")
//		println(myDFA.toRegExp.toNFA == myDFA)
	  }
	}
}