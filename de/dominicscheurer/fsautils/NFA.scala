package de.dominicscheurer.fsautils {
	import Types._
	import Conversions._
	import Helpers._
	import FSAMethods._

	class NFA[T <: State](
	    var alphabet: Set[Letter],
	    var states: Set[T],
	    var initialState: T,
	    var delta: ((T, Letter) => Option[Set[T]]),
	    var accepting: Set[T]) {
	  
	  require(states contains initialState)
	  require(accepting subsetOf states)
	  
	  def accepts(word: String): Boolean =
	    accepts(for (x <- word.toList) yield Symbol(x toString))
	  
	  def accepts(word: Word): Boolean = accepts(word, initialState)
	  
	  def accepts(word: Word, fromState: T): Boolean = word match {
	    case Nil => accepting contains fromState
	    case letter :: rest =>
	      delta(fromState, letter) match {
	        case None => false
	        case Some(listOfStates) =>
	          listOfStates.foldLeft(false)(
	            (result: Boolean, possibleSuccessor: T) =>
	              result || accepts(rest, possibleSuccessor))
	      }
	  }
	  
	  def * : NFA[T] = {
	    def deltaStar (state: T, letter: Letter) : Option[Set[T]] =
		  delta (state, letter) match {
	      	case None => None
	      	case Some(setOfStates) =>
	      	  Some(setOfStates ++ setOfStates.filter(accepting contains _)
	      	      .foldLeft(Set(initialState))((_,_) => Set(initialState)))
	      }
	    
	    (alphabet, states, initialState, deltaStar _, accepting) : NFA[T]
	  }
	  
	  def toDFA : DFA[set[T]] = {
	    val pStates = powerSet(states).map(setOfStates => set(setOfStates)) : Set[set[T]]
	    val pInitialState = set(Set(initialState)) : set[T]
	    val pAccepting = pStates.filter{
	      case (set(setOfStates)) => (setOfStates intersect accepting) nonEmpty
	      case _ => error("Impossible case")
	    } : Set[set[T]]
	    
	    def pDelta (state: set[T], letter: Letter) =
		  (state, letter) match {
	      	case (set(setOfStates), letter) =>
	      	  set(setOfStates.foldLeft(Set(): Set[T])((result, q) =>
	      	    delta(q, letter) match {
	      	      case None => result
	      	      case Some(setOfTargetStates) => result union setOfTargetStates
	      	    }))
	        case _ => error("Impossible case")
	      }
	    
	    (alphabet, pStates, pInitialState, pDelta _, pAccepting)
	  }
	  
	  override def toString = {
	    val indentSpace = "    "
	    val indentBeginner = "|"
	    val indent = "|" + indentSpace
	    val dindent = indent + indentSpace
	    var sb = new StringBuilder()
	    
	    sb ++= "NFA (Z,S,q0,D,A) with\n"
	    
	    sb ++= toStringUpToDelta(
	        indentBeginner,
	        indentSpace,
	        "Z", alphabet,
	        "S", states,
	        "q0", initialState,
	        "A", accepting);
	      
	    sb ++= indent ++= "D = {"
	    states.foreach(s =>
	      alphabet.filter(l => !delta(s,l).isEmpty).foreach(l =>
	        sb ++= "\n"
	           ++= dindent
	           ++= "("
	             ++= s.toString
	             ++= ","
	             ++= l.name
	             ++= ") => "
	             ++= (delta(s,l) match {
	             	   case None => "{}"
	             	   case Some(setOfStates) =>
	             	     setOfStates.foldLeft("")(
	             	         (result, aState) => result + aState.toString + " | ").stripSuffix(" | ")
	        	     })
	           ++= ","))
	    sb = sb.dropRight(1 - alphabet.isEmpty)
	    sb ++= "\n" ++= indent ++= "}\n"
	      
	    sb toString
	  }
		
	}
}