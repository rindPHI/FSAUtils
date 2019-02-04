package de.dominicscheurer.fsautils {
    import Types._
    import RegularExpressions._

    object Conversions {
        implicit def bool2int(b: Boolean) = if (b) 1 else 0

        implicit def DFAFromTuple(
            t: (Set[Letter], Set[State], State, ((State, Letter) => State), Set[State])): DFA = {
            new DFA(t._1, t._2, t._3, t._4, t._5)
        }

        implicit def NFAFromTuple(
            t: (Set[Letter], Set[State], State, ((State, Letter) => Set[State]), Set[State])): NFA = {
            new NFA(t._1, t._2, t._3, t._4, t._5)
        }

        implicit def DFAtoNFA[T](dfa: DFA): NFA =
            (dfa.alphabet, dfa.states, dfa.initialState,
                (state: State, letter: Letter) => {
                    try {
                        Set(dfa.delta(state, letter))
                    } catch {
                        case _: Throwable => Set(): Set[State]
                    }
                },
                dfa.accepting)

        implicit def REFromLetter(
            letter: Letter): RE = {
            L(letter)
        }

        // The following could be dangerous for short
        // regular expressions (conflicting implicits):
        implicit def SetFromStates(
            state: State): Set[State] =
            Set(state)

        implicit def SetFromStates(
            states: (State, State)) =
            Set(states._1, states._2)

        implicit def SetFromStates(
            states: (State, State, State)) =
            Set(states._1, states._2, states._3)
    }
}
