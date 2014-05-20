FSAUtils
========

Models of finite automata (DFA, NFA) with support of common operations and easily readable creation of objects.

The main goals of this project are:

* Support of easily readable definitions of finite automata (FA) and regular expressions
* Support of important basic operations on FA
* Adherence to the following coding guidelines aiming to assure correctness:
    * Simple and easily understandable code
    * Mostly adherence to the functional programming paradigm
    * Functional parts of the code (the core) closely follows abstract mathematical definitions of the respective operations

Features supported so far
-------------------------

* Creation of Deterministic Finite Automata (DFA)
* Creation of Nondeterministic Finite Automata (NFA)
* Determinization of NFA
* Creation of Regular Expressions (RE)
* Checking for acceptance of a word by an automaton
* Star-Operation for NFA
* Complement for DFA
* Implicit conversion of DFA to NFA
* Pretty-printing toString methods for DFA/NFA

Planned Features
----------------

* Concatenation, Star, Union, Intersection, Complement for DFA/NFA
* Minimization of DFA
* Determination of the language (RE) of a DFA/NFA
* Checking for equivalence of DFA/NFA/RE

Examples
--------

#### Creation of a DFA

````
val myDFA =
    dfa ('Z, 'S, 'q0, 'd, 'A) where
	    'Z  ==> Set('a, 'b)   and
	    'S  ==> Set(0, 1)     and
	    'q0 ==> 0             and
	    'A  ==> Set(0)        and
	    'd  ==> Delta(
              (0, 'a) -> 0,
              (0, 'b) -> 1,
              (1, 'a) -> 0,
              (1, 'b) -> 1
        )|

print("DFA accepts aaab: ")
println(myDFA accepts "aaab")
````

The above way to define a DFA has only recently been added and not yet been adopted to the NFA creation.
Another (better) way of defining the transition function is on the TODO list.

#### Creation of an NFA

````
def alphabet = Set('a, 'b)
def states = Set(q(0), q(1))             : States
def q0 = q(0)                            : State
def delta (state: State, letter: Letter) : NFADeltaResult =
  (state, letter) match {
	  case (q(0), 'a) => (q(0), q(1))
	  case (q(0), 'b) => q(0)
	  case _         => None
	}
def A = Set(q(1))                        : States

val myNFA = (alphabet, states, q0, delta _, A) : NFA

print("NFA accepts aaab: ")
println(myNFA accepts "aaab")
````

#### Star Operation for NFA

````
println((myNFA*) accepts "aaabaaab")
````

#### Determinization for NFA

````
println((myNFA toDFA) accepts "aaab")
````

#### Complement for DFA

````
println((!myDFA) accepts "aaab")
````

#### Pretty Printing

`println(myNFA toDFA)` yields:

```` 
DFA (Z,S,q0,d,A) with
|    Z = {a,b}
|    S = {{},{0},{1},{0,1}}
|    q0 = {0}
|    A = {{1},{0,1}}
|    d = {
|        ({},a) => {},
|        ({},b) => {},
|        ({0},a) => {0,1},
|        ({0},b) => {0},
|        ({1},a) => {},
|        ({1},b) => {},
|        ({0,1},a) => {0,1},
|        ({0,1},b) => {0}
|    }
````

#### Creation of RE

````
def myRegExp = (('a*) + ('b & ('b*) & 'a))* : RE
````
