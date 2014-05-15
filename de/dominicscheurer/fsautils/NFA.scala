package de.dominicscheurer.fsautils

import Types._
import Conversions._

class NFA(
    var alphabet: Set[Letter],
    var states: Set[State],
    var initialState: State,
    var delta: ((State, Letter) => Option[Set[State]]),
    var accepting: Set[State]) {
  
  require(states contains initialState)
  require(accepting subsetOf states)
  
  def accepts(word: String): Boolean =
    accepts(for (x <- word.toList) yield Symbol(x toString))
  
  def accepts(word: Word): Boolean = accepts(word, initialState)
  
  def accepts(word: Word, fromState: State): Boolean = word match {
    case Nil => accepting contains fromState
    case letter :: rest =>
      delta(fromState, letter) match {
        case None => false
        case Some(listOfStates) =>
          listOfStates.foldLeft(false)(
            (result: Boolean, possibleSuccessor: State) =>
              result || accepts(rest, possibleSuccessor))
      }
  }
  
  def * : NFA = {
    def deltaStar (state: State, letter: Letter) : Option[Set[State]] =
	  delta (state, letter) match {
      	case None => None
      	case Some(setOfStates) =>
      	  Some(setOfStates ++ setOfStates.filter(accepting contains _)
      	      .foldLeft(Set(initialState))((_,_) => Set(initialState)))
      }
    
    (alphabet, states, initialState, deltaStar _, accepting)
  }
	
}