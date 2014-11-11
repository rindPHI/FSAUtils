/*
 * Copyright 2014 Dominic Scheurer
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
    import Relations._
    
    import scala.annotation.tailrec
    import scala.xml.Node
    
    import Predef.{ any2stringadd => _, _ }

    class DFA(
        var alphabet: Set[Letter],
        var states: Set[State],
        var initialState: State,
        var delta: ((State, Letter) => State),
        var accepting: Set[State]) extends FSM {

        require(states contains initialState)
        require(accepting subsetOf states)

        def accepts(word: String): Boolean =
            accepts(for (x <- word.toList) yield Symbol(x toString))

        def accepts(word: Word): Boolean = accepts(word, initialState)

        def accepts(word: Word, fromState: State): Boolean = word match {
            case Nil => accepting contains fromState
            case letter :: rest => accepts(rest, delta(fromState, letter))
        }
        
        def extendAlphabet(newLetters: Set[Letter]): NFA = {
            (alphabet ++ newLetters, states, initialState, delta, accepting): DFA
        }

        def unary_! : DFA = new DFA(alphabet, states, initialState, delta, states -- accepting)

        def * : NFA = (this: NFA)*

        def ++(other: NFA): DFA =
            ((this: NFA) ++ other) toDFA

        def ++(other: DFA): DFA =
            this ++ (other: NFA)

        private def productAutomaton(other: DFA): DFA = {
            require(alphabet equals other.alphabet)
            
            val productStates = cartesianStateProduct(states, other.states)

            def productDelta(s: State, l: Letter): State = s match {
                case pair(s1, s2) => pair(delta(s1, l), other.delta(s2, l))
                case _ => error("Impossible case")
            }

            (alphabet, productStates, pair(initialState, other.initialState), productDelta _, Set(): Set[State])
        }

        def &(other: DFA): DFA = {
            require(alphabet equals other.alphabet)
            
            val intersAccepting = cartesianStateProduct(accepting, other.accepting)
            val product = productAutomaton(other)

            (alphabet, product.states, product.initialState, product.delta, intersAccepting)
        }
        
        def &(other: NFA): NFA = {
            require(alphabet equals other.alphabet)
            
            (this: NFA) & other
        }

        def |(other: DFA): DFA = {
            require(alphabet equals other.alphabet)
	            
            val unionAccepting = cartesianStateProduct(accepting, other.states) ++
                cartesianStateProduct(states, other.accepting)
            val product = productAutomaton(other)

            (alphabet, product.states, product.initialState, product.delta, unionAccepting)
        }
        
        def |(other: NFA): NFA = {
            require(alphabet equals other.alphabet)
            
            (this: NFA) | other
        }

        def \(other: DFA): DFA = {
            require(alphabet equals other.alphabet)
            
            this & (!other)
        }

        def \(other: NFA): DFA = {
            require(alphabet equals other.alphabet)
            
            this & (!(other toDFA))
        }

        def ==(other: DFA): Boolean =
            ((this \ other) isEmpty) && ((other \ this) isEmpty)

        def ==(other: NFA): Boolean =
            this == other.toDFA

        def isEmpty: Boolean = accepting.foldLeft(true)(
            (acc, s) => acc && !traverseDFS(List(initialState), List()).contains(s))

        def toRegExp: RE = {
            // Rename states: 1 .. n
            val renDFA = this getRenamedCopy 1
            renDFA.accepting.foldLeft(Empty(): RE)(
                (re, s) => re + renDFA.alpha(renDFA.states.size, renDFA initialState, s))
        }

        def getRenamedCopy(startVal: Int): DFA = {
            val emptyMap: Map[State, State] = Map()
            val renameMap: Map[State, State] =
                states.foldLeft(emptyMap) { (z, s) =>
                    z + (s -> q(z.size + startVal))
                }
            val reverseRenameMap = renameMap.map(_.swap)

            def deltaRen(state: State, letter: Letter): State =
                renameMap(delta(reverseRenameMap(state), letter))

            (alphabet,
                states.map(s => renameMap(s)),
                renameMap(initialState),
                deltaRen _,
                accepting.map(s => renameMap(s)))
        }

        def traverseDFS(toVisit: List[State], visited: List[State]): List[State] = {
            if (toVisit isEmpty) {
                List()
            } else {
                val next = toVisit head
                val succ = alphabet.map(l => delta(next, l)).toList diff toVisit diff visited

                next :: traverseDFS(toVisit.tail ++ succ, next :: visited)
            }
        }

        def minimize: DFA = {
            // first, remove unreachable states
            val reachableStates = states intersect (Set() ++ traverseDFS(List(initialState), List()))
            val reachableAccepting = accepting intersect reachableStates

            val rel = (reachableStates -- reachableAccepting)
                .foldLeft(new AntiReflSymmRel(): AntiReflSymmRel[State])(
                    (rel, s) => rel ++ reachableAccepting.foldLeft(Set(): Set[(State,State)])(
                        (set, a) => set + (s -> a)))

            val reachDFA = (alphabet, reachableStates, initialState, delta, reachableAccepting): DFA

            reachDFA minimize rel
        }

        @tailrec
        private def minimize(rel: AntiReflSymmRel[State]): DFA = {
            val cartProd = cartesianProduct(states, states)
            val differentPairs = cartProd.filter(p => p match {
                case (k, l) => rel.inRel(k, l) ||
                    alphabet.foldLeft(false)(
                        (acc, a) => acc || rel.inRel(delta(k, a), delta(l, a)))
                case _ => error("Should not happen")
            })

            val newRel = rel ++ differentPairs

            if (rel == newRel) {

                // Recursion anchor:
                // The distinguishing relation does not change in the next
                // iteration, so we construct the resulting automaton now
                // by "contracting" the states that are not distinguished

                val eqRel: EquivRel[State] =
                    new EquivRel() ++ cartProd.filter(p => !rel.inRel(p._1, p._2))

                val newStates = eqRel.equivalenceClasses.map(
                    setOfStates => set(setOfStates)): Set[State]

                val newInitialState = newStates.filter(state => state match {
                    case set(setOfStates: Set[State]) => setOfStates contains initialState
                    case _ => error("Impossible case.")
                }) head: State

                val newAccepting = newStates.filter(state => state match {
                    case set(setOfStates: Set[State]) => (setOfStates intersect accepting).nonEmpty
                    case _ => error("Impossible case.")
                }): Set[State]

                def newDelta(state: State, letter: Letter): State =
                    (state) match {
                        case set(setOfStates) => {
                            val someState = setOfStates head
                            val transResult = delta(someState, letter)

                            newStates.filter(state => state match {
                                case set(setOfStates: Set[State]) => setOfStates contains transResult
                                case _ => error("Impossible case.")
                            }) head
                        }
                        case _ => error("Impossible case.")
                    }

                (alphabet, newStates, newInitialState, newDelta _, newAccepting): DFA

            } else {

                minimize(newRel)

            }
        }

        private def alpha(k: Int, from: State, to: State): RE =
            if (k == 0) {
                val oneStepTransitions = alphabet
                    .filter(a => delta(from, a) == to)
                    .foldLeft(Empty(): RE)((re, a) => re + a)

                if (from == to) {
                    (Empty()*) + oneStepTransitions
                } else {
                    oneStepTransitions
                }
            } else {
                (from, to) match {
                    case (q(l), q(m)) =>
                        alpha(k - 1, q(l), q(m)) +
                            (alpha(k - 1, q(l), q(k)) &
                                (alpha(k - 1, q(k), q(k))*) &
                                alpha(k - 1, q(k), q(m)))

                    case _ => error("Should not happen: Call toRegExp() and not this method")
                }
            }

        override def toString = {
            val indentSpace = "    "
            val indentBeginner = "|"
            val indent = "|" + indentSpace
            val dindent = indent + indentSpace
            var sb = new StringBuilder()

            sb ++= "DFA (Z,S,q0,d,A) with\n"

            sb ++= toStringUpToDelta(
                indentBeginner,
                indentSpace,
                "Z", alphabet,
                "S", states,
                "q0", initialState,
                "A", accepting);

            sb ++= indent ++= "d = {"
            states.foreach(s =>
                alphabet.foreach(l =>
                    sb ++= "\n"
                        ++= dindent
                        ++= "("
                        ++= s.toString
                        ++= ","
                        ++= l.name
                        ++= ") => "
                        ++= delta(s, l).toString
                        ++= ","))
            sb = sb.dropRight(1 - alphabet.isEmpty)
            sb ++= "\n" ++= indent ++= "}\n"

            sb toString
        }
        
        override def toXml: scala.xml.Elem = {
            val renamed = getRenamedCopy(0)
            val alphabet = renamed.alphabet
            val states = renamed.states
            val initialState = renamed.initialState
            val delta = renamed.delta
            val accepting = renamed.accepting
<dfa>
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
                    <transition from={s.toString} trigger={l.name} to={delta(s,l).toString} />
        })}
    </delta>
	<accepting>
		{accepting.map { state => <state>{state.toString}</state> }}
	</accepting>
</dfa>
        }
    }
    
    object DFA {
        def Empty = {
            val alphabet = Set('a) : Set[Letter]
            val states = Set(q(0)) : Set[State]
            def initial = q(0)     : State
            val accepting = Set()  : Set[State]
            def delta(s: State, l: Letter): State = q(0)
            
            (alphabet, states, initial, delta _, accepting): DFA
        }
        
        def fromXml(node: Node): DFA = {
            /*
<dfa>
    <alphabet>
        <letter>a</letter>
        <letter>b</letter>
    </alphabet>
    <states>
        <state>1</state>
        <state>3</state>
        <state>5</state>
        <state>6</state>
        <state>4</state>
        <state>2</state>
    </states>
    <initialState>1</initialState>
    <delta>
        <transition from="1" trigger="b" to="3"/>
        <transition from="3" trigger="b" to="6"/>
        <transition from="6" trigger="b" to="6"/>
        <transition from="6" trigger="a" to="6"/>
        <transition from="1" trigger="a" to="2"/>
        <transition from="4" trigger="a" to="5"/>
        <transition from="2" trigger="a" to="1"/>
        <transition from="5" trigger="a" to="5"/>
        <transition from="2" trigger="b" to="4"/>
        <transition from="3" trigger="a" to="5"/>
        <transition from="4" trigger="b" to="6"/>
        <transition from="5" trigger="b" to="6"/>
    </delta>
    <accepting>
        <state>3</state>
        <state>4</state>
        <state>5</state>
    </accepting>
</dfa>
             */
            val alphabet = (node \ "alphabet" \ "letter") map {
                lNode => Symbol(lNode.text)
            }: Seq[Letter]
            
            val states = (node \ "states" \ "state") map {
                sNode => q(sNode.text.toInt)
            }: Seq[State]
            
            val initialState = q((node \ "initialState").text.toInt): State
            
            def delta(state: State, letter: Letter): State = {
                val transitions = (node \ "delta" \ "transition").foldLeft(Map[(State,Letter), State]())(
                    (map: Map[(State,Letter), State], elem: scala.xml.Node) =>
                            map + (
                                    (q((elem \ "@from").text.toInt), Symbol((elem \ "@trigger").text)) ->
                                        q((elem \ "@to").text.toInt))): Map[(State,Letter), State]
                
                transitions(state, letter)
            }
            
            val accepting = (node \ "accepting" \ "state") map {
                sNode => q(sNode.text.toInt)
            }: Seq[State]
            
            new DFA(alphabet.toSet, states.toSet, initialState, delta _, accepting.toSet)
        }
    }
}