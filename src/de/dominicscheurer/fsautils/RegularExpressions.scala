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
            
        def clean: RE = this match {
            case Star(inner) => inner match {
                // ((...)*)* => (...)*
                case Star(inner2) => Star(inner2 clean)
                // ({}* + XXX)* => (XXX)*
                case Or(Star(Empty()), rhs) => rhs clean
                // (XXX + {}*)* => (XXX)*
                case Or(lhs, Star(Empty())) => lhs clean
                case _ => Star(inner clean)
            }
            case Or(lhs, rhs) => lhs match {
                // {} + (...) => (...)
                case Empty() => rhs clean
                case Star(Empty()) => rhs match {
                    // {}* + (...)* => (...)*
                    case Star(rhsInner) => Star(rhsInner clean)
                    case _ => Or(lhs clean, rhs clean)
                }
                case _ => rhs match {
                    // (...) + {} => (...)
                    case Empty() => lhs clean
                    case Star(Empty()) => lhs match {
                        // (...)* + {}* => (...)*
                        case Star(lhsInner) => Star(lhsInner clean)
                        case _ => Or(lhs clean, rhs clean)
                    }
                    case _ => if (lhs equals rhs)
                        // XXX + XXX => XXX
                        lhs
                    else
                        Or(lhs clean, rhs clean)
                }
            }
            case Concat(lhs, rhs) => lhs match {
                // {} & (...) => (...)
                case Empty() => rhs clean
                case Or(Star(Empty()), lhsInner) => rhs match {
                    case Star(rhsInner) => {
                        val lhsInnerClean = lhsInner clean
                        val rhsInnerClean = rhsInner clean
                        
                        if (lhsInnerClean equals rhsInnerClean)
                            // (eps + XXX) & (XXX)* => (XXX)*
                            Star(rhsInnerClean)
                        else
                            Concat(lhs clean, rhs clean)
                    }
                    case _ => Concat(lhs clean, rhs clean)
                }
                case Or(lhsInner, Star(Empty())) => rhs match {
                    case Star(rhsInner) => {
                        val lhsInnerClean = lhsInner clean
                        val rhsInnerClean = rhsInner clean
                        
                        if (lhsInnerClean equals rhsInnerClean)
                            // (XXX + eps) & (XXX)* => (XXX)*
                            Star(rhsInnerClean)
                        else
                            Concat(lhs clean, rhs clean)
                    }
                    case _ => Concat(lhs clean, rhs clean)
                }
                case _ => rhs match {
                    // (...) + {} => (...)
                    case Empty() => lhs clean
                    case Or(rhsInner, Star(Empty())) => lhs match {
                        case Star(lhsInner) => {
                            val lhsInnerClean = lhsInner clean
                            val rhsInnerClean = rhsInner clean
                            
                            if (lhsInnerClean equals rhsInnerClean)
                                // (XXX)* & (XXX + eps) => (XXX)*
                                Star(lhsInnerClean)
                            else
                                Concat(lhs clean, rhs clean)
                        }
                        case _ => Concat(lhs clean, rhs clean)
                    }
                    case Or(Star(Empty()), rhsInner) => lhs match {
                        case Star(lhsInner) => {
                            val lhsInnerClean = lhsInner clean
                            val rhsInnerClean = rhsInner clean
                            
                            if (lhsInnerClean equals rhsInnerClean)
                                // (XXX)* & (eps + XXX) => (XXX)*
                                Star(lhsInnerClean)
                            else
                                Concat(lhs clean, rhs clean)
                        }
                        case _ => Concat(lhs clean, rhs clean)
                    } 
                    case _ => Concat(lhs clean, rhs clean)
                }
            }
            case _ => this
        }
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
                'Z ==> alph          and
                'S ==> Set(0)        and
                'q0 ==> 0            and
                'A ==> emptyAcc      and
                'd ==> DeltaRel(
                    emptyMap)        ||
        }
    }

    case class Star(re: RE) extends RE {
        override def toString = "(" + re.toString + ")*"
        override def alphabet = re.alphabet
        override def toNFAInt(alph: Set[Letter]) =
            if (re equals Empty())
                nfa('Z, 'S, 'q0, 'd, 'A)   where
                    'Z ==> alph            and
                    'S ==> Set(0)          and
                    'q0 ==> 0              and
                    'A ==> Set(0)          and
                    'd ==> DeltaRel(Map()) ||
            else
                (re toNFAInt alph)*
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
}