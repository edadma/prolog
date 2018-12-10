package xyz.hyperreal.prolog

import scala.collection.mutable
import scala.collection.mutable.ArrayStack


class VM( prog: Program ) {

  var trace = false
  var success = true
  var trail: List[Variable] = Nil
  implicit var vars: VarMap = _

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

    def map = vars map { case (k, v) => (k, v.eval) } toMap
  }

  case class State( dataStack: List[Any], pc: Int, frame: Frame, trail: List[Variable] )

  class Frame( size: Int, val ret: Int ) {
    val vars = new Array[Variable]( size )
  }

  val choiceStack = new ArrayStack[State]

  var dataStack: List[Any] = Nil
  var pc = -1
  var frame: Frame = new Frame( 0, -1 )

  def interp( goal: TermAST ) = {
    success = true
    vars = new VarMap

    goal match {
      case CompoundAST( _, name, args ) if prog.defined( name, args.length ) =>
        pushFrame
        args foreach interpTerm
        call( prog.procedure(name, args.length).entry )
        run
      case CompoundAST( _, name, args ) if Builtins.predicates contains functor( name, args.length ) =>
        args foreach interpTerm
        Builtins.predicates(functor(name, args.length))( this )
        Some( vars.map )
      case CompoundAST( pos, name, args ) => pos.error( s"rule $name/${args.length} not defined" )
      case AtomAST( _, name ) if prog.defined( name, 0 ) =>
        pushFrame
        call( prog.procedure( name, 0).entry )
        run
      case AtomAST( _, name ) if Builtins.predicates contains functor( name, 0 ) =>
        Builtins.predicates(functor(name, 0))( this )
        Some( vars.map )
      case AtomAST( pos, name ) => pos.error( s"rule $name/0 not defined" )
      case _ => goal.pos.error( "expected a rule" )
    }
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

  def pushFrame = push( frame )

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

  def popBoolean = pop.asInstanceOf[Boolean]

  def push( d: Any ): Unit = dataStack = d :: dataStack

  def call( entry: Int ): Unit = {
    push( pc )
    pc = entry
  }

  def fail: Unit = {
    if (choiceStack nonEmpty)
      choiceStack pop match {
        case State( _dataStack, _pc, _frame, _trail ) =>
          dataStack = _dataStack
          pc = _pc
          frame = _frame

          var p = trail

          while (p.head ne _trail.head) {
            p.head.unbind
            p = p.tail
          }
      }
    else
      success = false
  }

  def execute {
    val inst = prog(pc)

    if (trace)
      println( inst, dataStack )

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
          case v: Variable => frame.vars(n) = v
          case v => frame.vars(n) = new Variable {bind( v )}
        }
      case FunctorInst( f ) =>
        top match {
          case c: CompoundData if c.functor == f =>
          case _ => fail
        }
      case DupInst => push( top )
      case EqInst => push( popData == popData )
      case BranchIfInst( disp ) =>
        if (popBoolean)
          pc += disp
      case FailInst => fail
      case ChoiceInst( disp ) => choiceStack push State( dataStack, pc + disp, frame, trail )
      case CallInst( entry ) => call( entry )
      case DropInst => pop
      case PushFrameInst => pushFrame
      case FrameInst( vars ) => frame = new Frame( vars, popInt )
      case PredicateInst( pred ) => pred( this )
      case UnifyInst =>
        val const = popData

        popValue match {
          case v: Variable => v bind const
          case v => if (v != const) fail
        }
    }
  }

  def run = {
    if (pc >= 0 && success)
      while (pc >= 0) {
        execute
      }
    else
      success = false

    if (trace)
      println( dataStack )

    if (success)
      Some( vars.map )
    else
      None
  }

  object Variable {
    private var count = 0
  }

  class Variable {
    Variable.count += 1
    trail ::= this

    val num = Variable.count
    var instantiated = false
    var value: Any = _
    var binding: Variable = _

    def bind( v: Variable ): Unit = {
      instantiated = false
      binding = v
    }

    def bind( v: Any ): Unit = {
      instantiated = true
      value = v
    }

    def unbind: Unit = {
      instantiated = false
      binding = null
    }

    def eval: Any =
      if (instantiated)
        value
      else if (binding ne null)
        binding.eval
      else
        this

    override def toString: String =
      eval match {
        case v: Variable => s"_V${v.num}"
        case v => v.toString
      }
  }

}