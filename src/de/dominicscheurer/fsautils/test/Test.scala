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

package de.dominicscheurer.fsautils.test

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import de.dominicscheurer.fsautils.FSA_DSL
import de.dominicscheurer.fsautils.Types._
import de.dominicscheurer.fsautils.DFA
import de.dominicscheurer.fsautils.NFA

class Test extends FlatSpec with Matchers with FSA_DSL {
    
//                                                      ,..._
//     ,-'''-.            ,-----.             ,-'''-. ,'     `.
//    /       `.    b    / /   \ `.    b     /       `.       | a,b
//    |   2    |-------->||  4  | |--------->|   6    |       |
//    `.      /          `.`._,' /           `.      /.<-:_,_/
//      `...-'             `...-'`. a       /\ `...-'  
//       /|\                       `._    ,'     |
//        |a                          `.,'       | b
//        |                           ,'`.       |
//       \|/                        ,'b   `.     |
//     ,-'''-.             ,-----. -        `v,-;--..
//    /       `.    b     /,'   `.`.    a    / /    `:.
//--->|   1    |--------->||  3  | |-------->||  5   ||
//    `.      /           `\_   ,'/          `.\    //_,
//      `...-'              `----'             `:,-' '|.
//                                              |     ` \
//                                               \      | a
//                                                `" ../
    
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
                
         
//                                     
//        ,---.                 ,-;===:-.
//      ,'     `.              /,'     `.\
//     .'       '.      b     .:'       ':.
//---->|    1    |----------->||    2    ||
//      \       /              \\       //
//       `.._,,'                `:.._,,;'
//        ,'  /'`.              _/---'''/\
//       |      `.             .'        |
//       `       |             |.        |
//        `-...,'               ` -__   ,'
//          a                       '`''
//                                  a
                
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
                
//           a                a
//         __..._          _,.....              ____
//        /'     \        /'      `|      a,b ,'   '`.
//       /'      /        |       ,|          /      |
//        ..  `,/,.        ' .  `.i_          L_   |__
//         ,-''-:_           ,-ii-._           ,-''-._
//       .'       \   b    .','  `-.\    b   .'       \
// ----->|   1    |------->|/  2   '.------->|    3   |
//       \        /        \\      //        \        /
//        `._  _,'          `:._,,;'          `._  _,'
//           `'                `'                `'
                
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
                
    "A DFA" should "equal its expected minimization" in {
        assert(dfa1.minimize == dfa1)
        assert(dfa1.minimize == dfa1eqMinDFA)
    }
                
    it should "equal the equivalent NFA" in
        assert(dfa1 == dfa1eqNFA)
                
    it should "not be changed by and/or operations with itself" in {
        assert((dfa1 & dfa1) == dfa1)
        assert((dfa1 | dfa1) == dfa1)
    }

    it should "be stable under double negation" in
        assert(!(!dfa1) == dfa1)

    it should "be stable under serialization" in {
        assert(DFA.fromXml(dfa1.toXml) == dfa1)
        assert(DFA.fromXml(dfa1eqNFA.toDFA.toXml) == dfa1eqNFA)
    }
    
    // This may take a while:
    it should "be stable under Regular Expression building" in
        assert(dfa1.toRegExp.toNFA == dfa1)

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

    it should "be stable under serialization" in {
        assert(NFA.fromXml(dfa1eqNFA.toXml) == dfa1eqNFA)
    }

    it should "be stable under Regular Expression building" in
        assert(dfa1eqNFA.toRegExp.toNFA == dfa1eqNFA)
    
    "The result of an NFA minus itself" should "be empty" in
        assert((dfa1eqNFA \ dfa1eqNFA).isEmpty)

    "The cut of an NFA with its star" should "be the NFA again" in
        assert((dfa1eqNFA & (dfa1eqNFA*)) == dfa1eqNFA)

    "RegExp operations" should "behave according to the corresponding automaton operations" in {
        val re = dfa1eqNFA.toRegExp
        assert((re & re).toNFA == (dfa1eqNFA ++ dfa1eqNFA))
        assert((re + re).toNFA == (dfa1eqNFA | dfa1eqNFA))
        assert((re*).toNFA     == (dfa1eqNFA*))
    }
    
}