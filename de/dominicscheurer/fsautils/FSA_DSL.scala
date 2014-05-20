package de.dominicscheurer.fsautils {
    import Types._
    
	class FSA_DSL[T <: State] {
		case class DFABuilder(
		      t: (Symbol, Symbol, Symbol, Symbol, Symbol)) {
		    // The elements that need to be filled
		    var alphabet : Option[Set[Letter]]              = None
			var states   : Option[Set[T]]          = None
			var q0       : Option[T]                    = None
			var delta    : Option[(T, Letter) => T] = None
			var A        : Option[Set[T]]          = None
		    
			// Connectors for definition: "where" and "and"
		    def where = (input: (Symbol, Any)) => (input._2 match {
		        case symbols: SymbolSet => input._1 match {
		            case t._1 => { alphabet = Some(symbols.set); this }
		        }
		        case stateSet: StateSet => input._1 match {
		            case t._2 => { states = Some(stateSet.set); this }
		            case t._5 => { A = Some(stateSet.set);      this }
		        }
		        case state: T => input._1 match {
		            case t._3 => { q0 = Some(state); this }
		        }
		        case func: DeltaFun => input._1 match {
		            case t._4 => { delta = Some(func.fun); this }
		        }
		    })
		    
		    def and = where
		    
		    def testDone : Boolean =
		        alphabet.isDefined &&
		        states.isDefined   &&
		        q0.isDefined       &&
		        delta.isDefined    &&
		        A.isDefined
		    
		    def | : DFA[T] =
		        if (testDone)
		        	new DFA(alphabet.get, states.get, q0.get, delta.get, A.get)
		        else
		            error("Some values of the DFA are still undefined")
		}
		
		// Starting point: dfa function
		def dfa(t: (Symbol, Symbol, Symbol, Symbol, Symbol)) =
		    DFABuilder(t)
		    
		// Syntactic Sugar
		case class SymbolWrapper(s: Symbol) {
		    def ==>(vals: SymbolSet) = (s, vals)
		    def ==>(vals: StateSet) = (s, vals)
		    def ==>(aval: State) = (s, aval)
		    def ==>(afun: DeltaFun) = (s, afun)
		}
		    
		implicit def SymbToSymbWrapper(s: Symbol) = SymbolWrapper(s)
		
		case class StateSet(set: Set[T])
		case class StringSet(set: Set[String])
		case class SymbolSet(set: Set[Symbol])
		case class DeltaFun(fun: ((T, Letter) => T))
		implicit def is(set: Set[T]) = StateSet(set)
		implicit def sts(set: Set[String]) = StringSet(set)
		implicit def sys(set: Set[Symbol]) = SymbolSet(set)
		implicit def dfun(fun: ((T, Letter) => T)) = DeltaFun(fun)
	}
}