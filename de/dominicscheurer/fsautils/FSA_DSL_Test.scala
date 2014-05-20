package de.dominicscheurer.fsautils {
	import de.dominicscheurer.fsautils._
  
	import Types._
	import Conversions._
	import RegularExpressions._
	
	import Predef.{any2stringadd => _, _}

	object FSA_DSL_Test extends FSA_DSL {
		import FSABuilder._
		
		def main(args: Array[String]) {
		    val myDFA =
			    dfa ('Z, 'S, 'q0, 'd, 'A) where
				    'Z  ==> Set('a, 'b)   and
				    'S  ==> Set(0, 1)     and
				    'q0 ==> 0             and
				    'A  ==> Set(0)        and
				    'd  ==> Delta(
						  (0, 'a) -> 0,
						  (0, 'b) -> 1,
						  (1, 'a) -> 0,
						  (1, 'b) -> 1
					)|
			
			println(myDFA.accepts("baa"))
			
		    val myNFA =
			    nfa ('Z, 'S, 'q0, 'd, 'A) where
			        'Z  ==> Set('a, 'b)   and
			        'S  ==> Set(0, 1)     and
			        'q0 ==> 0             and
			        'A  ==> Set(1)        and
			        'd  ==> Delta(
			              (0, 'a) -> Set(0, 1),
			              (0, 'b) -> Set(0)
			        )||
			
			println(myNFA.accepts("aaaaaa"))
		}
	}
}