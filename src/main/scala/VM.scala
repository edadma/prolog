package xyz.hyperreal.prolog

import scala.collection.mutable.ArrayStack


class VM( prog: Program ) {

  case class State( dataStack: List[Data], pc: Int, frame: Int )

  class Frame( size: Int ) {
    val vars = new Array[Any]( size )
  }

  val choiceStack = new ArrayStack[State]

  var dataStack: List[Any] = Nil
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
      args(i) = popData

    push( CompoundData(f, args.toVector) )
  }

  def top = dataStack.head

  def pop = {
    val res = top

    dataStack = dataStack.tail
    res
  }

  def popData = pop.asInstanceOf[Data]

  def push( d: Any ): Unit = dataStack = d :: dataStack

  def call( entry: Int ): Unit = {

  }

  def fail: Unit = {

  }

  def execute {
    val inst = prog(pc)

    pc += 1

    inst match {
      case PushAtomicInst( d ) => push( d )
      case PushVarInst( n ) =>
      case PushCompoundInst( f ) => pushCompound( f )
      case PushElementInst( n ) => push( pop.asInstanceOf[Compound].element(n) )
      case ReturnInst =>
      case VarBindInst( n ) =>
      case FunctorInst( f ) =>
        top match {
          case c: CompoundData if c.functor == f =>
          case _ => fail
        }
      case DupInst => push( top )
      case EqInst => pop == pop
      case BranchIfInst( disp ) => pc += disp
      case FailInst => fail
      case ChoiceInst( disp ) =>
      case CallInst( entry ) => call( entry )
      case DropInst => pop
    }
  }

}