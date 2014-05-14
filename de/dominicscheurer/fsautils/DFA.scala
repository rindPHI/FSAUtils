package de.dominicscheurer.fsautils

import Types._
import Conversions._

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
}