package de.dominicscheurer.fsautils {
	import de.dominicscheurer.fsautils._
  
	import Types._
	import Conversions._
	import RegularExpressions._
	
	import Predef.{any2stringadd => _, _}

	object FSA_DSL_Test extends FSA_DSL[q] {
		import DFABuilder._
		
		def main(args: Array[String]) {
		    val myDFA =
			    dfa ('Z, 'S, 'q0, 'd, 'A)   where
				    'Z  ==> Set('a, 'b)     and
				    'S  ==> Set(q(0), q(1)) and
				    'q0 ==> q(0)            and
				    'A  ==> Set(q(0))       and
				    'd  ==> ((s: State, l: Letter) =>
					  (s, l) match {
						  case (q(0), 'a) => q(0)
						  case (q(0), 'b) => q(1)
						  case (q(1), 'a) => q(0)
						  case (q(1), 'b) => q(1)
					  })|
			
			println(myDFA.accepts("aaaa"))
		}
	}
}