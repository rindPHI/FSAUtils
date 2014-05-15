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
  
  override def toString = {
    val indentSpace = "    "
    val indent = "|" + indentSpace
    val dindent = indent + indentSpace
    var sb = new StringBuilder()
    
    sb ++= "DFA (Z,S,q0,d,A) with\n"
      
    sb ++= indent ++= "Z = {"
    alphabet.foreach(s => sb ++= s.name ++= ",")
    sb = sb.dropRight(1 - alphabet.isEmpty)
    sb ++= "}\n"
    
    sb ++= indent ++= "S = {"
    states.foreach(s => sb ++= s.toString() ++= ",")
    sb = sb.dropRight(1 - states.isEmpty)
    sb ++= "}\n"
      
    sb ++= indent ++= "q0 = " ++= initialState.toString ++= "\n"
    
    sb ++= indent ++= "A = {"
    accepting.foreach(s => sb ++= s.toString() ++= ",")
    sb = sb.dropRight(1 - accepting.isEmpty)
    sb ++= "}\n"
      
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