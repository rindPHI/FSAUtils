package de.dominicscheurer.fsautils.test

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import de.dominicscheurer.fsautils.FSA_DSL
import de.dominicscheurer.fsautils.Types._

class Test extends FlatSpec with Matchers with FSA_DSL {
    
    val dfa1 =
            dfa ('Z, 'S, 'q0, 'd, 'A)         where
                'Z  ==> Set('a, 'b)           and
                'S  ==> Set(1, 2, 3, 4, 5, 6) and
                'q0 ==> 1                     and
                'A  ==> Set(3, 4, 5)          and
                'd  ==> Delta(
                      (1, 'a) -> 2,
                      (1, 'b) -> 3,
                      (2, 'a) -> 1,
                      (2, 'b) -> 4,
                      (3, 'a) -> 5,
                      (3, 'b) -> 6,
                      (4, 'a) -> 5,
                      (4, 'b) -> 6,
                      (5, 'a) -> 5,
                      (5, 'b) -> 6,
                      (6, 'a) -> 6,
                      (6, 'b) -> 6
                )|
                
    val dfa1eqNFA =
            nfa ('Z, 'S, 'q0, 'd, 'A) where
                'Z  ==> Set('a, 'b)   and
                'S  ==> Set(1, 2)      and
                'q0 ==> 1             and
                'A  ==> Set(2)        and
                'd  ==> Delta(
                      (1, 'a) -> Set(1),
                      (1, 'b) -> Set(2),
                      (2, 'a) -> Set(2)
                )||
                
    val dfa1eqMinDFA =
            dfa ('Z, 'S, 'q0, 'd, 'A)         where
                'Z  ==> Set('a, 'b)           and
                'S  ==> Set(1, 2, 3) and
                'q0 ==> 1                     and
                'A  ==> Set(2)          and
                'd  ==> Delta(
                      (1, 'a) -> 1,
                      (1, 'b) -> 2,
                      (2, 'a) -> 2,
                      (2, 'b) -> 3,
                      (3, 'a) -> 3,
                      (3, 'b) -> 3
                )|
          
    /////// DFA ///////
                
    "A DFA" should "equal its expected minimization" in
        assert(dfa1.minimize == dfa1eqMinDFA)
                
    it should "equal the equivalent NFA" in
        assert(dfa1 == dfa1eqNFA)
                
    it should "not be changed by and/or operations with itself" in {
        assert((dfa1 & dfa1) == dfa1)
        assert((dfa1 | dfa1) == dfa1)
    }

    it should "be stable under double negation" in
        assert(!(!dfa1) == dfa1)
    
    // The following test consumes very much memory and tends to produce
    // OutOfMemory errors (possibly very large power set construction).
    // The situation gets better with
    //   dfa1.minimize.toRegExp.toNFA
    // due to a shorter RE, but it is still problematic. Maybe one should go
    // for something like RE minimization?
//    it should "be stable under Regular Expression building" in
//        assert(dfa1.toRegExp.toNFA == dfa1)

    "The result of a DFA minus itself" should "be empty" in
        assert((dfa1 \ dfa1).isEmpty)

    "The cut of a DFA with its star" should "be the DFA again" in
        assert((dfa1 & (dfa1*).toDFA) == dfa1)
    
    /////// NFA ///////
                
    "An NFA" should "equal the equivalent DFA" in
        assert(dfa1eqNFA == dfa1)
                
    it should "not be changed by and/or operations with itself" in {
        assert((dfa1eqNFA & dfa1eqNFA) == dfa1eqNFA)
        assert((dfa1eqNFA | dfa1eqNFA) == dfa1eqNFA)
    }

    it should "be stable under double negation" in
         assert(!(!dfa1eqNFA) == dfa1eqNFA)

    it should "be stable under Regular Expression building" in
        assert(dfa1eqNFA.toRegExp.toNFA == dfa1eqNFA)
    
    "The result of an NFA minus itself" should "be empty" in
        assert((dfa1eqNFA \ dfa1eqNFA).isEmpty)
    
}