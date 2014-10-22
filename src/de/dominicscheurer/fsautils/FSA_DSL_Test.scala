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
	object FSA_DSL_Test extends FSA_DSL {
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