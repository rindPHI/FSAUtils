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
    import Helpers._
    import FSAMethods._
    import RegularExpressions._

    class NFA(
        var alphabet: Set[Letter],
        var states: Set[State],
        var initialState: State,
        var delta: ((State, Letter) => Set[State]), //Power Set instead of Relation
        var accepting: Set[State]) {

        require(states contains initialState)
        require(accepting subsetOf states)

        def accepts(word: String): Boolean =
            accepts(for (x <- word.toList) yield Symbol(x toString))

        def accepts(word: Word): Boolean = accepts(word, initialState)

        def accepts(word: Word, fromState: State): Boolean = word match {
            case Nil => accepting contains fromState
            case letter :: rest =>
                delta(fromState, letter).foldLeft(false)(
                    (result: Boolean, possibleSuccessor: State) =>
                        result || accepts(rest, possibleSuccessor))
        }
        
        def extendAlphabet(newLetters: Set[Letter]): NFA = {
            (alphabet ++ newLetters, states, initialState, delta, accepting)
        }

        def unary_! : DFA = !(this toDFA)

        def * : NFA = {
        	// Avoid name clash for case where new start state has to be added
            if ((!this accepts "") && states.contains(q(0))) {
                (getRenamedCopy(1)*): NFA
            }
            
            def deltaStar(state: State, letter: Letter): Set[State] = {
                val deltaRes = delta(state, letter): Set[State]
                deltaRes ++ deltaRes.filter(accepting contains _)
                            .foldLeft(Set(): Set[State])((_, _) => Set(initialState))
            }

            if (!this accepts "") {
                val newInitialState = q(0)
                
                def deltaStar2(state: State, letter: Letter): Set[State] =
                    if (state == newInitialState) {
                        deltaStar(initialState, letter)
                    } else {
	                    deltaStar(state, letter)
                    }
                
                (alphabet, states + newInitialState, newInitialState, deltaStar2 _, accepting + newInitialState)
                
            } else {
                
                (alphabet, states, initialState, deltaStar _, accepting)
                
            }
        }

        def ++(otherOrig: NFA): NFA = {
            // Rename before concatenation to avoid state name clash
            val thisR = this getRenamedCopy 0
            val other = otherOrig getRenamedCopy states.size

            thisR concat other
        }

        private def productAutomaton(other: NFA): NFA = {
            require(alphabet equals other.alphabet)
            
            val productStates = cartesianStateProduct(states, other.states)

            def productDelta(s: State, l: Letter): Set[State] = s match {
                case pair(s1, s2) => {
                    cartesianStateProduct(delta(s1, l), other.delta(s2, l))
                }
                case _ => error("Impossible case")
            }

            (alphabet, productStates, pair(initialState, other.initialState), productDelta _, Set(): Set[State])
        }

        def &(other: NFA): NFA = {
            require(alphabet equals other.alphabet)
            
            val intersAccepting = cartesianStateProduct(accepting, other.accepting)
            val product = productAutomaton(other)

            (alphabet, product.states, product.initialState, product.delta, intersAccepting)
        }

        def |(other: NFA): NFA = {
            require(alphabet equals other.alphabet)
            
            if (   !(states intersect other.states).isEmpty
                || (states union other.states).contains(q(0)))
                getRenamedCopy(1) | other.getRenamedCopy(states.size + 1)
            else {
                def newDelta(s: State, l: Letter): Set[State] =
                    if (s == q(0))
                        delta(initialState, l) ++ other.delta(other.initialState, l)
                    else
                        if (states.contains(s))
                            delta(s, l)
                        else
                            other.delta(s,l)
                
                if ((this accepts "") || (other accepts ""))
                    (alphabet, states ++ other.states + q(0), q(0), newDelta _, accepting ++ other.accepting + q(0))
                else
                    (alphabet, states ++ other.states + q(0), q(0), newDelta _, accepting ++ other.accepting)
            }
        }

        def \(other: NFA): DFA =
            (this toDFA) \ (other toDFA)

        def ==(other: NFA): Boolean =
            (this toDFA) == (other toDFA)

        def ==(other: DFA): Boolean =
            (this toDFA) == (other toDFA)

        def isEmpty: Boolean = (this toDFA) isEmpty

        def toRegExp: RE = (this toDFA) toRegExp

        //TODO: This power set construction seems to be problematic
        //      regarding memory consumption already for some automata
        //      with only two states!?! (See Test.scala)
        def toDFA: DFA = {
            val pStates = powerSet(states).map(setOfStates => set(setOfStates)): States
            val pInitialState = set(Set(initialState)): State
            val pAccepting = pStates.filter {
                case (set(setOfStates)) => (setOfStates intersect accepting) nonEmpty
                case _ => error("Impossible case")
            }

            def pDelta(state: State, letter: Letter) =
                (state, letter) match {
                    case (set(setOfStates), letter) =>
                        set(setOfStates.foldLeft(Set(): States)((result, q) =>
                            result union delta(q, letter)
                    ))
                    case _ => error("Impossible case")
                }

            (alphabet, pStates, pInitialState, pDelta _, pAccepting)
        }

        def getRenamedCopy(startVal: Int): NFA = {
            val emptyMap: Map[State, State] = Map()
            val renameMap: Map[State, State] =
                states.foldLeft(emptyMap) { (z, s) =>
                    z + (s -> q(z.size + startVal))
                }
            val reverseRenameMap = renameMap.map(_.swap)

            def deltaRen(state: State, letter: Letter): Set[State] =
                delta(reverseRenameMap(state), letter).map(s => renameMap(s))

            (alphabet,
                states.map(s => renameMap(s)),
                renameMap(initialState),
                deltaRen _,
                accepting.map(s => renameMap(s)))
        }

        private def concat(other: NFA): NFA = {
            if (this accepts "") {

                val noEpsAccepting = accepting - initialState
                val concatNoEps = ((alphabet, states, initialState, delta, noEpsAccepting): NFA) concat other

                val statesCup = concatNoEps.states + q(-1)

                def deltaCup(state: State, letter: Letter): Set[State] =
                    if (state == q(-1))
                        concatNoEps.delta(initialState, letter) ++ other.delta(other.initialState, letter)
                    else
                        concatNoEps.delta(state, letter)

                ((alphabet ++ other.alphabet, statesCup, q(-1), deltaCup _, concatNoEps.accepting): NFA) getRenamedCopy 0

            } else {

                val statesCup = states ++ other.states

                def deltaCup(state: State, letter: Letter): Set[State] =
                    if (other.states contains state)
                        other.delta(state, letter) // Delta_2
                    else { // Delta_1 \cup Delta_{1->2}
                        val deltaRes = delta(state, letter)
                        deltaRes ++ deltaRes.filter(accepting contains _)
                            .foldLeft(Set(): Set[State])((_, _) => Set(other.initialState))
                    }

                (alphabet ++ other.alphabet, statesCup, initialState, deltaCup _, other.accepting)

            }
        }

        override def toString = {
            val indentSpace = "    "
            val indentBeginner = "|"
            val indent = "|" + indentSpace
            val dindent = indent + indentSpace
            var sb = new StringBuilder()

            sb ++= "NFA (Z,S,q0,D,A) with\n"

            sb ++= toStringUpToDelta(
                indentBeginner,
                indentSpace,
                "Z", alphabet,
                "S", states,
                "q0", initialState,
                "A", accepting);

            sb ++= indent ++= "D = {"
            states.foreach(s =>
                alphabet.filter(l => !delta(s, l).isEmpty).foreach(l =>
                    sb ++= "\n"
                        ++= dindent
                        ++= "("
                        ++= s.toString
                        ++= ","
                        ++= l.name
                        ++= ") => "
                        ++= (if (delta(s, l).isEmpty) "{}"
                            else
                                delta(s, l).foldLeft("")(
                                    (result, aState) => result + aState.toString + " | ").stripSuffix(" | "))
                        ++= ","))
            sb = sb.dropRight(1 - alphabet.isEmpty)
            sb ++= "\n" ++= indent ++= "}\n"

            sb toString
        }

    }
}