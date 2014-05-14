package de.dominicscheurer.fsautils

import Types._
import Conversions._
import RegularExpressions._

import Predef.{any2stringadd => _, _}

object Main {
  
  def main(args: Array[String]) {
    
    // DFA
	def alphabet = Set('a, 'b)
	def states = Set('q0, 'q1)
	def q0 = 'q0
	def delta (state: State, letter: Letter) =
	  (state, letter) match {
		  case ('q0, 'a) => 'q0
		  case ('q0, 'b) => 'q1
		  case ('q1, 'a) => 'q0
		  case ('q1, 'b) => 'q1
		}
	def A = Set('q0)
	
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
		  case ('q0, 'a) => ('q0, 'q1)
		  case ('q0, 'b) => 'q0
		  case _         => None
		}
	def A_NFA = Set('q1)
	
	val myNFA = (alphabet, states, q0, deltaNFA _, A_NFA) : NFA
	
	print("NFA accepts aaab: ")
	println(myNFA accepts "aaab")
	print("NFA accepts aaaba: ")
	println(myNFA accepts "aaaba")
	
    // NFA Operations
	def delta_NFAOp (state: State, letter: Letter) : NFADeltaResult =
	  (state, letter) match {
		  case ('q0, 'a) => 'q0
		  case ('q0, 'b) => 'q1
		  case _         => None
		}
	
	val myNFA2 = (alphabet, states, q0, delta_NFAOp _, A_NFA) : NFA
	
	print("NFA2 accepts aaab: ")
	println(myNFA2 accepts "aaab")
	print("NFA2 accepts aaabaaab: ")
	println(myNFA2 accepts "aaabaaab")
	
	print("NFA2* accepts aaab: ")
	println((myNFA2*) accepts "aaab")
	print("NFA2* accepts aaabaaab: ")
	println((myNFA2*) accepts "aaabaaab")
	
	// RegExp
	val myRegExp = (('a*) + ('b & ('b*) & 'a))* : RE	
  }
}