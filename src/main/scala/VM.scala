package xyz.hyperreal.prolog

import scala.collection.mutable
import scala.collection.mutable.ArrayStack


class VM( prog: Program ) {

  class VarMap {
    val vars = new mutable.HashMap[String, Variable]

    def apply( name: String ) =
      vars get name match {
        case None =>
          val v = new Variable

          vars(name) = v
          v
        case Some( v ) => v
      }
  }

  case class State( dataStack: List[Any], pc: Int, frame: Frame )

  class Frame( size: Int, val ret: Int ) {
    val vars = new Array[Variable]( size )
  }

  val choiceStack = new ArrayStack[State]

  var dataStack: List[Any] = Nil
  var pc = 0
  var frame: Frame = _

  def interp( goal: TermAST ) {
    implicit val vars = new VarMap

    goal match {
      case CompoundAST( _, name, args ) if prog.defined( name, args.length ) =>
        args foreach interpTerm
        call( prog.procedure(name, args.length).entry )
      case CompoundAST( _, name, args ) if Builtins.predicates contains functor( name, args.length ) =>
        args foreach interpTerm
        Builtins.predicates( functor(name, args.length) )
      case CompoundAST( pos, name, args ) => pos.error( s"rule $name/${args.length} not defined" )
      case AtomAST( _, name ) if prog.defined( name, 0 ) => call( prog.procedure( name, 0).entry )
      case AtomAST( _, name ) if Builtins.predicates contains functor( name, 0 ) =>
        Builtins.predicates( functor( name, 0) )
      case AtomAST( pos, name ) => pos.error( s"rule $name/0 not defined" )
      case _ => goal.pos.error( "expected a rule" )
    }

    run
  }

  def interpTerm( term: TermAST )( implicit vars: VarMap ): Unit =
    term match {
      case CompoundAST( pos, name, args ) =>
        args foreach interpTerm
        pushCompound( Functor(Symbol(name), args.length) )
      case AtomAST( pos, name ) => push( AtomData(Symbol(name)) )
      case WildcardAST( pos ) => pos.error( "wildcard not allowed here" )
      case VariableAST( pos, name ) => push( vars(name) )
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

  def popValue =
    pop match {
      case v: Variable => v.eval
      case v => v
    }

  def popData = popValue.asInstanceOf[Data]

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
        push(
          frame.vars(n) match {
            case null =>
              val v = new Variable

              frame.vars(n) = v
              v
            case v => v
          }
        )
      case PushCompoundInst( f ) => pushCompound( f )
      case PushElementInst( n ) => push( popValue.asInstanceOf[Compound].element(n) )
      case ReturnInst =>
        pc = frame.ret
        frame = pop.asInstanceOf[Frame]
      case VarBindInst( n ) =>
        popValue match {
          case v: Variable => push( v )
          case v =>
        }
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
      case PredicateInst( pred ) => pred( this )
    }
  }

  def run: Unit = {
    while (true) {
      execute
    }
  }

}