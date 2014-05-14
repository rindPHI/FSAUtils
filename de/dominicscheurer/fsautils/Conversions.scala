package de.dominicscheurer.fsautils

import Types._
import RegularExpressions._

object Conversions {
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
  
  implicit def DFAtoNFA(dfa: DFA) : NFA =
    (dfa.alphabet, dfa.states, dfa.initialState,
        (state: State, letter: Letter) => Some(Set(dfa.delta(state, letter))),
        dfa.accepting)
  
  implicit def REFromLetter(
      letter: Letter) : RE = {
    L(letter)
  }
  
  // The following could be dangerous for short
  // regular expressions (conflicting implicits):
  implicit def OptionSetFromLetters(
      letter: Letter) : Option[Set[Letter]] =
    Some(Set(letter))
  
  implicit def OptionSetFromLetters(
      letters: (Letter, Letter)) =
    Some(Set(letters._1, letters._2))

  implicit def OptionSetFromLetters(
      letters: (Letter, Letter, Letter)) =
    Some(Set(letters._1, letters._2, letters._3))
}