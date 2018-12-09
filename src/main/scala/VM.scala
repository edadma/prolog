package xyz.hyperreal.prolog

import scala.collection.mutable
import scala.collection.mutable.ArrayStack


class VM( prog: Program ) {

  class VarMap {
    val vars = new mutable.HashMap[String, Variable]

    def apply( name: String ) = {

    }
  }

  case class State( dataStack: List[Any], pc: Int, frame: Frame )

  class Frame( size: Int, val ret: Int ) {
    val vars = new Array[Any]( size )
  }

  val choiceStack = new ArrayStack[State]

  var dataStack: List[Any] = Nil
  var pc = 0
  var frame: Frame = _

  def interp( goal: TermAST ) {
    implicit val vars = new mutable.HashMap[String, Variable]

    goal match {
      case CompoundAST( _, name, args ) if prog.exists( name, args.length ) =>
        args foreach interpTerm
        call( prog.procedure(name, args.length).entry )
      case CompoundAST( pos, name, args ) => pos.error( s"rule $name/${args.length} not defined" )
      case AtomAST( _, name ) if prog.exists( name, 0 ) => call( prog.procedure( name, 0).entry )
      case AtomAST( pos, name ) => pos.error( s"rule $name/0 not defined" )
      case _ => goal.pos.error( "expected a rule" )
    }

    run
  }

  def interpTerm( term: TermAST )( implicit vars: mutable.HashMap[String, Variable] ): Unit =
    term match {
      case CompoundAST( pos, name, args ) =>
        args foreach interpTerm
        pushCompound( Functor(Symbol(name), args.length) )
      case AtomAST( pos, name ) => push( AtomData(Symbol(name)) )
      case WildcardAST( pos ) => pos.error( "wildcard not allowed here" )
      case VariableAST( pos, name ) => push( vars.lookup(name) )
      case IntegerAST( pos, v ) => push( IntegerData(v) )
      case FloatAST( pos, v ) => push( FloatData(v) )
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

  def popInt = pop.asInstanceOf[Int]

  def push( d: Any ): Unit = dataStack = d :: dataStack

  def call( entry: Int ): Unit = {
    push( pc )
    pc = entry
  }

  def fail: Unit = {
    if (choiceStack nonEmpty)
      choiceStack pop match {
        case State( _dataStack, _pc, _frame ) =>
          dataStack = _dataStack
          pc = _pc
          frame = _frame
      }
    else
      sys.error( "no more choice points" )
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
        pc = frame.ret
        frame = pop.asInstanceOf[Frame]
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
      case ChoiceInst( disp ) => choiceStack push State( dataStack, pc + disp, frame )
      case CallInst( entry ) => call( entry )
      case DropInst => pop
      case FrameInst( vars ) =>
        val ret = popInt

        push( frame )
        frame = new Frame( vars, ret )
    }
  }

  def run: Unit = {
    while (true) {
      execute
    }
  }

}