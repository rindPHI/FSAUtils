package de.dominicscheurer.fsautils {
	import Types._
	import Conversions._
	
	object FSAMethods {
		def toStringUpToDelta[T <: State](
		    indentBeginner: String,
		    indentSpace: String,
		    alphabetDesignator: String,
		    alphabet: Set[Letter],
		    statesDesignator: String,
		    states: Set[T],
		    initialStateDesignator: String,
		    initialState: T,
		    acceptingDesignator: String,
		    accepting: Set[T]) : String = {
		  
			val indent = indentBeginner + indentSpace
		    val dindent = indent + indentSpace
		    var sb = new StringBuilder()
		      
		    sb ++= indent ++= alphabetDesignator ++= " = {"
		    alphabet.foreach(s => sb ++= s.name ++= ",")
		    sb = sb.dropRight(1 - alphabet.isEmpty)
		    sb ++= "}\n"
		    
		    sb ++= indent ++= statesDesignator ++= " = {"
		    states.foreach(s => sb ++= s.toString() ++= ",")
		    sb = sb.dropRight(1 - states.isEmpty)
		    sb ++= "}\n"
		      
		    sb ++= indent ++= initialStateDesignator ++= " = " ++= initialState.toString ++= "\n"
		    
		    sb ++= indent ++= acceptingDesignator ++= " = {"
		    accepting.foreach(s => sb ++= s.toString() ++= ",")
		    sb = sb.dropRight(1 - accepting.isEmpty)
		    sb ++= "}\n"
		      
		    sb.toString
		}
	}
}