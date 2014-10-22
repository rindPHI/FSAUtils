package de.dominicscheurer.fsautils {
	import Types._
	import Conversions._
	import FSAMethods._
	import Helpers._
	
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
	    val intersStates = cartesianStateProduct(states, other.states)

	    def intersDelta(s: State, l: Letter): State = s match {
	      case pair(s1,s2) => pair(delta(s1,l), other.delta(s2,l))
	      case _ => error("Impossible case")
	    }
	    
	    (alphabet, intersStates, pair(initialState, other.initialState), intersDelta _, Set(): Set[State])
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