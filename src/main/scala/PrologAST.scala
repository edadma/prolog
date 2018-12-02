package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.Reader


abstract class PrologAST

case class SourceAST( clauses: List[ClauseAST] ) extends PrologAST

case class ClauseAST( term: TermAST ) extends PrologAST

abstract class TermAST extends PrologAST {
  val pos: Reader
}

case class CompoundAST( pos: Reader, name: String, args: List[TermAST] ) extends TermAST
case class AtomAST( pos: Reader, name: String ) extends TermAST
case class VariableAST( pos: Reader, var name: String ) extends TermAST
case class WildcardAST( pos: Reader ) extends TermAST

abstract class NumericAST extends TermAST {
  val pos: Reader
  val n: Number
}

case class IntegerAST( pos: Reader, n: Integer ) extends NumericAST
case class FloatAST( pos: Reader, n: java.lang.Double ) extends NumericAST
