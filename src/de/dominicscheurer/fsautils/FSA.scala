/* Copyright 2014 Dominic Scheurer
 * 
 * This file is part of FSAUtils.
 * 
 * FSAUtils is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * FSAUtils is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *  
 * You should have received a copy of the GNU General Public License
 * along with FSAUtils. If not, see <http://www.gnu.org/licenses/>.
 */

package de.dominicscheurer.fsautils {
	import Types._
	import Conversions._
	
	object FSAMethods {
		def toStringUpToDelta(
		    indentBeginner: String,
		    indentSpace: String,
		    alphabetDesignator: String,
		    alphabet: Set[Letter],
		    statesDesignator: String,
		    states: Set[State],
		    initialStateDesignator: String,
		    initialState: State,
		    acceptingDesignator: String,
		    accepting: Set[State]) : String = {
		  
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