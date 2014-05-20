package de.dominicscheurer.fsautils {
	import de.dominicscheurer.fsautils._
  
	import Types._
	import Conversions._
	import RegularExpressions._
	
	import Predef.{any2stringadd => _, _}

	object FSA_DSL_Test extends FSA_DSL {
		import DFABuilder._
		
		def main(args: Array[String]) {
		    val myDFA =
			    dfa ('Z, 'S, 'q0, 'd, 'A) where
				    'Z  ==> Set('a, 'b)   and
				    'S  ==> Set(0, 1)     and
				    'q0 ==> 0             and
				    'A  ==> Set(0)        and
				    'd  ==> ((s: Int, l: Letter) =>
					  (s, l) match {
						  case (0, 'a) => 0
						  case (0, 'b) => 1
						  case (1, 'a) => 0
						  case (1, 'b) => 1
					  })|
			
			println(myDFA.accepts("b"))
		}
	}
}