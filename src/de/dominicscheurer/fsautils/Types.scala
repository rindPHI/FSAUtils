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
	import Conversions._
  
	object Types {
		type Letter = Symbol
		type Word = List[Letter]
		type NFADeltaResult = Set[State]
        type States = Set[State]
		
		abstract sealed class State
		
		case class q(i: Int) extends State {
		  override def toString = i toString
		}
		
		case class set(s: Set[State]) extends State {
		  override def toString = {
		    var sb = new StringBuilder
		    
		    sb ++= s.foldLeft("{")((result, state) => result + state.toString + ",")
		    sb = sb.dropRight(1 - s.isEmpty)
		    sb ++= "}"
		      
		    sb toString
		  }
		}
		
		case class pair(s1: State, s2: State) extends State
		
		abstract class FSM {
           def isDFA = this.isInstanceOf[DFA]
           def isNFA = this.isInstanceOf[NFA]
           def asDFA: Option[DFA] =
               if (isDFA)
                   Some(this.asInstanceOf[DFA])
               else
                   None
           def asNFA: Option[NFA] =
               if (isNFA)
                   Some(this.asInstanceOf[NFA])
               else
                   None
        }
	}
}