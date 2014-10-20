package de.dominicscheurer.fsautils {
	import de.dominicscheurer.fsautils._
  
	import Types._
	import Conversions._
	import RegularExpressions._
	
	import Predef.{any2stringadd => _, _}
  
	object Main {  
	  def main(args: Array[String]) {
	    
	    // DFA
	    def alphabet = Set('a, 'b)
		def states = Set(q(0), q(1))             : States
		def q0 = q(0)                            : State
		def delta (state: State, letter: Letter) =
		  (state, letter) match {
			  case (q(0), 'a) => q(0)
			  case (q(0), 'b) => q(1)
			  case (q(1), 'a) => q(0)
			  case (q(1), 'b) => q(1)
			}
		def A = Set(q(0))                        : States
		
		val myDFA = (alphabet, states, q0, delta _, A) : DFA
		
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
		
		val myNFA = (alphabet, states, q0, deltaNFA _, A_NFA) : NFA
		
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
		println(myNFA ++ myNFA2);
		
		
		val test = myNFA ++ myNFA2
		println(test accepts "bb")
		
		// RegExp
		def myRegExp = (('a*) + ('b & ('b*) & 'a))* : RE
	  }
	}
}