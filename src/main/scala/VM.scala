package xyz.hyperreal.prolog

import scala.collection.mutable.ArrayStack


class VM( prog: Program ) {

  case class State( dataStack: List[Data], pc: Int, frame: Int )

  val choiceStack = new ArrayStack[State]

  var dataStack: List[Data] = Nil
  var pc = 0
  var frame = 0

  def interp( goal: TermAST ) {
    implicit val vars = new Vars

    goal match {
      case CompoundAST( _, name, args ) if prog.exists( name, args.length ) =>
        args foreach interpTerm
        call( prog.procedure(name, args.length).entry )
      case CompoundAST( pos, name, args ) => pos.error( s"rule $name/${args.length} not defined" )
      case AtomAST( _, name ) if prog.exists( name, 0 ) => call( prog.procedure( name, 0).entry )
      case AtomAST( pos, name ) => pos.error( s"rule $name/0 not defined" )
      case _ => goal.pos.error( "expected a rule" )
    }
  }

  def interpTerm( term: TermAST )( implicit vars: Vars ): Unit =
    term match {
      case CompoundAST( pos, name, args ) =>
        args foreach interpTerm
        pushCompound( Functor(Symbol(name), args.length) )
      case AtomAST( pos, name ) => push( AtomData(Symbol(name)) )
      case WildcardAST( pos ) => pos.error( "wildcard not allowed here" )
      case VariableAST( pos, name ) => PushVarInst( vars.num(name) )
      case IntegerAST( pos, v ) => PushAtomicInst( IntegerData(v) )
      case FloatAST( pos, v ) => PushAtomicInst( FloatData(v) )
    }

  def pushCompound( f: Functor ): Unit = {
    val args = new Array[Data]( f.arity )

    for (i <- f.arity - 1 to 0 by -1)
      args(i) = pop

    push( CompoundData(f, args.toVector) )
  }

  def top = dataStack.head

  def pop = {
    val res = top

    dataStack = dataStack.tail
    res
  }

  def push( d: Data ): Unit = dataStack = d :: dataStack

  def call( entry: Int ): Unit = {

  }

  def execute( inst: Instruction ) =
    inst match {
      case PushAtomicInst( d ) => push( d )
    }

}