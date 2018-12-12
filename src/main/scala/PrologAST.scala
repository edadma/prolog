package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.Reader


abstract class PrologAST

case class SourceAST( clauses: List[ClauseAST] ) extends PrologAST

case class ClauseAST( term: TermAST ) extends PrologAST

abstract class TermAST extends PrologAST {
  val pos: Reader
}

case class StructureAST( pos: Reader, name: String, args: List[TermAST] ) extends TermAST
case class AtomAST( pos: Reader, name: String ) extends TermAST
case class VariableAST( pos: Reader, var name: String ) extends TermAST
case class WildcardAST( pos: Reader ) extends TermAST

abstract class NumericAST extends TermAST {
  val pos: Reader
  val v: Number
}

case class IntegerAST( pos: Reader, v: Integer ) extends NumericAST
case class FloatAST( pos: Reader, v: java.lang.Double ) extends NumericAST
