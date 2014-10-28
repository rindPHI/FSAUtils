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
import Conversions._

object RegularExpressions {
    sealed abstract class RE extends FSA_DSL {
        def *(): RE = Star(this)
        def +(rhs: RE): RE = Or(this, rhs)
        def &(rhs: RE): RE = Concat(this, rhs)

        def alphabet: Set[Letter]

        def toNFA: NFA = toNFAInt(alphabet)
        def toNFAInt(alph: Set[Letter]): NFA

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
            .replace("{}", "\u00D8") // emptyset
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
        override def alphabet = Set(l)
        override def toNFAInt(alph: Set[Letter]) =
            nfa('Z, 'S, 'q0, 'd, 'A) where
                'Z ==> alph and
                'S ==> Set(0, 1) and
                'q0 ==> 0 and
                'A ==> Set(1) and
                'd ==> Delta(
                    (0, l) -> Set(1))||
    }

    case class Empty() extends RE {
        override def toString = "{}"
        override def alphabet = Set()
        override def toNFAInt(alph: Set[Letter]) = {
            val emptyAcc: Set[Int] = Set()
            val emptyMap: Map[(Int, Letter), Set[Int]] = Map()
            nfa('Z, 'S, 'q0, 'd, 'A) where
                'Z ==> alph and
                'S ==> Set(0) and
                'q0 ==> 0 and
                'A ==> emptyAcc and
                'd ==> DeltaRel(
                    emptyMap) ||
        }
    }

    case class Star(re: RE) extends RE {
        override def toString = "(" + re.toString + ")*"
        override def alphabet = re.alphabet
        override def toNFAInt(alph: Set[Letter]) = (re toNFAInt alph)*
    }

    case class Or(lhs: RE, rhs: RE) extends RE {
        override def toString = "(" + lhs.toString + " + " + rhs.toString + ")"
        override def alphabet = lhs.alphabet ++ rhs.alphabet
        override def toNFAInt(alph: Set[Letter]) = ((lhs toNFAInt alph) | (rhs toNFAInt alph)): NFA
    }

    case class Concat(lhs: RE, rhs: RE) extends RE {
        override def toString = "(" + lhs.toString + " & " + rhs.toString + ")"
        override def alphabet = lhs.alphabet ++ rhs.alphabet
        override def toNFAInt(alph: Set[Letter]) = ((lhs toNFAInt alph) ++ (rhs toNFAInt alph)): NFA
    }

    def main(args: Array[String]) = {
        // From here, experimental
        
        val res2p1 = (Empty()*) + 'a: RE
        val res2p2 = ('b & ('b*)): RE
        val res2p3 = 'a: RE
//        
//        println("\nPart 1: {}* + a")
//        println(res2p1.toNFA.toDFA.minimize.getRenamedCopy(0))
//        println("\nPart 2: b & b*")
//        println(res2p2.toNFA)
//        println("\nPart 2: b & b* [manual]")
//        println(('b.toNFA.toDFA.minimize.getRenamedCopy(0) ++ ('b*).toNFA.toDFA.minimize.getRenamedCopy(0)).minimize.getRenamedCopy(0))
//        println("\n> b:")
//        println('b.toNFA.toDFA.minimize.getRenamedCopy(0))
//        println("\n> b*")
//        println(('b*).toNFA.toDFA.minimize.getRenamedCopy(0))
//        println("\nPart 3: a")
//        println(res2p3.toNFA)
//        println("\nPart 2 & Part 3")
        val interm1 = res2p2.toNFA ++ res2p3.toNFA
//        println(interm1)
//        println("\nPart 1 + (Part 2 & Part 3)")
        println((res2p1.toNFA.extendAlphabet(Set('a, 'b)) | interm1).toDFA.minimize.getRenamedCopy(0))
    }
}