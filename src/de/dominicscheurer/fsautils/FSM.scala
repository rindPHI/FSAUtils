package de.dominicscheurer.fsautils {

    import Types._
    import Conversions._
    import RegularExpressions._

    abstract class FSM {
        def isDFA = this.isInstanceOf[DFA]
        def isNFA = this.isInstanceOf[NFA]
        def asDFA: Option[DFA] =
            if (isDFA)
                Some(this.asInstanceOf[DFA])
            else
                None
        def asNFA: Option[NFA] =
            if (isNFA)
                Some(this.asInstanceOf[NFA])
            else
                None

        def toXml: scala.xml.Elem

        def toPrettyXml: String = {
            val printer = new scala.xml.PrettyPrinter(80, 2)
            printer.format(toXml)
        }

        def toStringUpToDelta(
            indentBeginner: String,
            indentSpace: String,
            alphabetDesignator: String,
            alphabet: Set[Letter],
            statesDesignator: String,
            states: Set[State],
            initialStateDesignator: String,
            initialState: State,
            acceptingDesignator: String,
            accepting: Set[State]): String = {

            val indent = indentBeginner + indentSpace
            val dindent = indent + indentSpace
            var sb = new StringBuilder()

            sb ++= indent ++= alphabetDesignator ++= " = {"
            alphabet.foreach(s => sb ++= s.name ++= ",")
            sb = sb.dropRight(1 - alphabet.isEmpty)
            sb ++= "}\n"

            sb ++= indent ++= statesDesignator ++= " = {"
            states.foreach(s => sb ++= s.toString() ++= ",")
            sb = sb.dropRight(1 - states.isEmpty)
            sb ++= "}\n"

            sb ++= indent ++= initialStateDesignator ++= " = " ++= initialState.toString ++= "\n"

            sb ++= indent ++= acceptingDesignator ++= " = {"
            accepting.foreach(s => sb ++= s.toString() ++= ",")
            sb = sb.dropRight(1 - accepting.isEmpty)
            sb ++= "}\n"

            sb.toString
        }

        def accepts(word: String): Boolean

        def accepts(word: Word): Boolean

        def accepts(word: Word, fromState: State): Boolean

        def extendAlphabet(newLetters: Set[Letter]): NFA
        
        def adjustAlphabet(other: FSM): NFA =
            if (other.isDFA)
                extendAlphabet(other.asDFA.get.alphabet)
            else
                extendAlphabet(other.asNFA.get.alphabet)

        def unary_! : FSM

        def * : FSM

        def ++(other: DFA): FSM
        def ++(other: NFA): FSM
        def ++(other: FSM): FSM =
            if (other isDFA)
                this ++ other.asDFA.get
            else
                this ++ other.asNFA.get

        def &(other: DFA): FSM
        def &(other: NFA): FSM
        def &(other: FSM): FSM =
            if (other isDFA)
                this & other.asDFA.get
            else
                this & other.asNFA.get

        def |(other: DFA): FSM
        def |(other: NFA): FSM
        def |(other: FSM): FSM =
            if (other isDFA)
                this | other.asDFA.get
            else
                this | other.asNFA.get

        def \(other: DFA): FSM
        def \(other: NFA): FSM
        def \(other: FSM): FSM =
            if (other isDFA)
                this \ other.asDFA.get
            else
                this \ other.asNFA.get

        def ==(other: DFA): Boolean
        def ==(other: NFA): Boolean
        def ==(other: FSM): Boolean =
            if (other isDFA)
                this == other.asDFA.get
            else
                this == other.asNFA.get

        def isEmpty: Boolean

        def toRegExp: RE
    }
}
