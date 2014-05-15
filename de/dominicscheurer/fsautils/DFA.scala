package de.dominicscheurer.fsautils

import Types._
import Conversions._
import FSAMethods._

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