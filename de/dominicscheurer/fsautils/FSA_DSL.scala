package de.dominicscheurer.fsautils {
    import Types._
    
	class FSA_DSL {
		case class DFABuilder(
		      t: (Symbol, Symbol, Symbol, Symbol, Symbol)) {
		    // The elements that need to be filled
		    var alphabet : Option[Set[Letter]]              = None
			var states   : Option[States]                   = None
			var q0       : Option[State]                    = None
			var delta    : Option[(State, Letter) => State] = None
			var A        : Option[States]                   = None
		    
			// Connectors for definition: "where" and "and"
		    def where = (input: (Symbol, Any)) => (input._2 match {
		        case symbols: SymbolSet => input._1 match {
		            case t._1 => { alphabet = Some(symbols.set); this }
		        }
		        case stateSet: IntSet => input._1 match {
		            case t._2 => { states = Some(stateSet.set.map(s => q(s))); this }
		            case t._5 => { A = Some(stateSet.set.map(s => q(s)));      this }
		        }
		        case state: Int => input._1 match {
		            case t._3 => { q0 = Some(q(state)); this }
		        }
		        case func: DeltaFun => input._1 match {
		            case t._4 => {
		                def myDelta (s: State, l: Letter): State = s match {
		                    case q(i) => q(func.fun(i, l))
		                    case _    => error("Should not occur")
		                }
		                delta = Some(myDelta)
		                this
		            }
		        }
		    })
		    
		    def and = where
		    
		    def testDone : Boolean =
		        alphabet.isDefined &&
		        states.isDefined   &&
		        q0.isDefined       &&
		        delta.isDefined    &&
		        A.isDefined
		    
		    def | : DFA =
		        if (testDone)
		        	new DFA(alphabet.get, states.get, q0.get, delta.get, A.get)
		        else
		            error("Some values of the DFA are still undefined")
		    
		    def done = |
		}
		
		// Starting point: dfa function
		def dfa(t: (Symbol, Symbol, Symbol, Symbol, Symbol)) =
		    DFABuilder(t)
		    
		// Syntactic Sugar
		case class SymbolWrapper(s: Symbol) {
		    def ==>(vals: SymbolSet) = (s, vals)
		    def ==>(vals: IntSet) = (s, vals)
		    def ==>(aval: Int) = (s, aval)
		    def ==>(afun: DeltaFun) = (s, afun)
		}
		    
		implicit def SymbToSymbWrapper(s: Symbol) = SymbolWrapper(s)
		
		case class IntSet(set: Set[Int])
		case class StringSet(set: Set[String])
		case class SymbolSet(set: Set[Symbol])
		case class DeltaFun(fun: ((Int, Letter) => Int))
		implicit def is(set: Set[Int]) = IntSet(set)
		implicit def sts(set: Set[String]) = StringSet(set)
		implicit def sys(set: Set[Symbol]) = SymbolSet(set)
		implicit def dfun(fun: ((Int, Letter) => Int)) = DeltaFun(fun)
	}
}