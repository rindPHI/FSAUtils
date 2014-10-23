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

package de.dominicscheurer.fsautils

import Types._

object RegularExpressions {
  sealed abstract class RE {
     def *(): RE = Star(this)
     def +(rhs: RE): RE = Or(this, rhs)
     def &(rhs: RE): RE = Concat(this, rhs)
     
     /**
      * cleanString does some post processing on the
      * toString method in order to make the output better
      * readable. However, you naturally achieve better
      * correctness guarantees without this method (since
      * this is just string manipulation with regular expressions).
      */
     def cleanString = recClean(toString)
     
     private def recClean(s: String): String = {
       val cleanRes = clean(s)
       if (s equals cleanRes) {
         s
       } else {
         recClean(cleanRes)
       }
     }
     
     private def clean(s: String) = s
     	.replace("{} + ", "")
     	.replace("({})*", "\u025B") // epsilon
     	.replace("{}", "\u00D8")    // emptyset
     	.replace("**", "*")
     	.replaceAll("""'([a-z])""", "$1")
     	.replaceAll("""\(([a-z])\)""", "$1")
     	.replaceAll("""\(\(([^\(\)]+)\)\)\*""", "($1)*")
     	.replaceAll("""\(\u025B \+ ([^\(\)]+)\)\*""", "($1)*")
     	.replaceAll(""" [&\+] \u00D8""", "")
     	.replaceAll("""\u00D8 [&\+] """, "")
     	.replaceAll("""\(([a-z\u025B])\)([\*]?)""", "$1$2")
     	.replaceAll("""\(\(([^\(\)]+)\)\)""", "($1)")
     	.replaceAll("""\(([a-z])\*\)""", "$1*")
  }
  
  case class L(l: Letter) extends RE {
    override def toString = l toString
  }
  case class Empty() extends RE {
    override def toString = "{}"
  }
  case class Star(re: RE) extends RE {
    override def toString = "(" + re.toString + ")*"
  }
  case class Or(lhs: RE, rhs: RE) extends RE {
    override def toString = "(" + lhs.toString + " + " + rhs.toString + ")"
  }
  case class Concat(lhs: RE, rhs: RE) extends RE {
    override def toString = "(" + lhs.toString + " & " + rhs.toString + ")"
  }
}