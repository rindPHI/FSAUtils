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
    import RegularExpressions._

    class NFA(
        var alphabet: Set[Letter],
        var states: Set[State],
        var initialState: State,
        var delta: ((State, Letter) => Set[State]), //Power Set instead of Relation
        var accepting: Set[State]) extends FSM {

        require(states contains initialState)
        require(accepting subsetOf states)

        def acceptsEmptyWord: Boolean =
            accepting contains initialState // There are no epsilon-translations
        
        def accepts(word: String): Boolean =
            accepts(for (x <- word.toList) yield Symbol(x toString))

        def accepts(word: Word): Boolean = accepts(word, initialState)

        def accepts(word: Word, fromState: State): Boolean = word match {
            case Nil => accepting contains fromState
            case letter :: rest =>
                (alphabet contains letter) &&
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
            if (!(this acceptsEmptyWord) && states.contains(q(0))) {
                (getRenamedCopy(1)*): NFA
            }
            
            def deltaStar(state: State, letter: Letter): Set[State] = {
                val deltaRes = delta(state, letter): Set[State]
                if (deltaRes exists ( accepting contains _ ))
                    deltaRes + initialState
                else
                    deltaRes
            }

            if (!(this acceptsEmptyWord)) {
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

        def ++(otherOrig: DFA): NFA = {
            this ++ (otherOrig: NFA)
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

        def &(other: DFA): NFA = {
            require(alphabet equals other.alphabet)
            
            this & (other: NFA)
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
                
                if ((this acceptsEmptyWord) || (other acceptsEmptyWord))
                    (alphabet, states ++ other.states + q(0), q(0), newDelta _, accepting ++ other.accepting + q(0))
                else
                    (alphabet, states ++ other.states + q(0), q(0), newDelta _, accepting ++ other.accepting)
            }
        }

        def |(other: DFA): NFA = {
            require(alphabet equals other.alphabet)
            
            this & (other: NFA)
        }

        def \(other: NFA): DFA =
            (this toDFA) \ (other toDFA)

        def \(other: DFA): DFA =
            (this toDFA) \ other

        def ==(other: NFA): Boolean =
            (this toDFA) == (other toDFA)

        def ==(other: DFA): Boolean =
            (this toDFA) == other

        def isEmpty: Boolean = (this toDFA) isEmpty

        def toRegExp: RE = (this toDFA) toRegExp

        def toDFA: DFA = {
            val pInitialState = set(Set(initialState)): State

            def pDelta(state: State, letter: Letter) =
                (state, letter) match {
                    case (set(setOfStates), letter) =>
                        set(setOfStates.foldLeft(Set(): States)((result, q) =>
                            result union delta(q, letter)
                    ))
                    case _ => error("Impossible case")
                }
            

            val interm = (alphabet, Set(pInitialState), pInitialState, pDelta _, Set(pInitialState)): DFA
            val pStates = interm.traverseDFS(List(pInitialState), List()).toSet
            val pAccepting = pStates.filter {
                case (set(setOfStates)) => (setOfStates intersect accepting) nonEmpty
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
        
        def removeUnreachable: NFA = {
            val reachableStates = states intersect (traverseDFS(List(initialState), List()).toSet)
            val reachableAccepting = accepting intersect reachableStates

            (alphabet, reachableStates, initialState, delta, reachableAccepting): NFA
        }

        private def concat(other: NFA): NFA = {
            if (this acceptsEmptyWord) {

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
                        if (deltaRes exists ( accepting contains _ ))
                            deltaRes + other.initialState
                        else
                            deltaRes
                            }

                (alphabet ++ other.alphabet, statesCup, initialState, deltaCup _, other.accepting)

            }
        }

        private def traverseDFS(toVisit: List[State], visited: List[State]): List[State] = {
            if (toVisit isEmpty) {
                List()
            } else {
                val next = toVisit head
                val succ = alphabet.foldLeft(Set(): Set[State])(
                               (acc, l) => acc ++ delta(next, l)
                           ).toList diff toVisit diff visited

                next :: traverseDFS(toVisit.tail ++ succ, next :: visited)
            }
        }
        
        override def toXml: scala.xml.Elem = {
            val renamed = getRenamedCopy(0)
            val alphabet = renamed.alphabet
            val states = renamed.states
            val initialState = renamed.initialState
            val delta = renamed.delta
            val accepting = renamed.accepting
<nfa>
    <alphabet>
        {alphabet.map { letter => <letter>{letter.toString.replaceFirst("'", "")}</letter> }}
    </alphabet>
    <states>
        {states.map { state => <state>{state.toString}</state> }}
    </states>
    <initialState>{initialState}</initialState>
    <delta>
        {cartesianProduct(states,alphabet).map({
                case (s,l) =>
                    delta(s,l).map {
                        to => <transition from={s.toString} trigger={l.name} to={to.toString} />
                    }
        })}
    </delta>
    <accepting>
        {accepting.map { state => <state>{state.toString}</state> }}
    </accepting>
</nfa>
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
    
    object NFA {
        def fromXml(node: scala.xml.Node): NFA = {
            /*
<nfa>
  <alphabet>
    <letter>a</letter>
    <letter>b</letter>
  </alphabet>
  <states>
    <state>0</state>
    <state>1</state>
  </states>
  <initialState>0</initialState>
  <delta>
    <transition from="0" trigger="a" to="0"/>
    <transition from="0" trigger="b" to="1"/>
    <transition from="1" trigger="a" to="1"/>
  </delta>
  <accepting>
    <state>1</state>
  </accepting>
</nfa>
             */
            val alphabet = (node \ "alphabet" \ "letter") map {
                lNode => Symbol(lNode.text)
            }: Seq[Letter]
            
            val states = (node \ "states" \ "state") map {
                sNode => q(sNode.text.toInt)
            }: Seq[State]
            
            val initialState = q((node \ "initialState").text.toInt): State
            
            def delta(state: State, letter: Letter): Set[State] = {
                val transitions = (node \ "delta" \ "transition").foldLeft(Map[(State,Letter), Set[State]]())(
                    (map: Map[(State,Letter), Set[State]], elem: scala.xml.Node) => {
                        val from = q((elem \ "@from").text.toInt): State
                        val trigger = Symbol((elem \ "@trigger").text): Letter
                        val to = q((elem \ "@to").text.toInt): State
                        
                        if (map contains (from,trigger)) {
                            val oldRes = map(from, trigger)
                            (map - ((from,trigger))) + ((from, trigger) -> (oldRes + to))
                        } else {
                            map + ((from, trigger) -> Set(to)): Map[(State,Letter), Set[State]]
                        }
                    })
                
                if (transitions contains(state, letter))
                    transitions(state, letter)
                else
                    Set()
            }
            
            val accepting = (node \ "accepting" \ "state") map {
                sNode => q(sNode.text.toInt)
            }: Seq[State]
            
            new NFA(alphabet.toSet, states.toSet, initialState, delta _, accepting.toSet)
        }
    }
}