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
    val vars = Array.fill[Variable]( size )( new Variable )
  }

  val choiceStack = new ArrayStack[State]

  var dataStack: List[Any] = Nil
  var pc = -1
  var frame: Frame = new Frame( 0, -1 )

  def interp( goal: TermAST ) = {
    success = true
    vars = new VarMap
    trail = Nil

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
        pushStructure( Functor(Symbol(name), args.length) )
      case AtomAST( pos, name ) => push( Symbol(name) )
      case WildcardAST( pos ) => pos.error( "wildcard not allowed here" )
      case VariableAST( pos, name ) => push( vars(name) )
      case IntegerAST( pos, v ) => push( v )
      case FloatAST( pos, v ) => push( v )
    }

  def pushFrame = push( frame )

  def pushStructure(f: Functor ): Unit = {
    val args = new Array[Any]( f.arity )

    for (i <- f.arity - 1 to 0 by -1)
      args(i) = popValue

    push( Structure(f, args.toVector) )
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

  def popInt = pop.asInstanceOf[Int]

  def popBoolean = pop.asInstanceOf[Boolean]

  def push( d: Any ): Unit = dataStack ::= d

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

          while (p.nonEmpty && (_trail.isEmpty || (p.head ne _trail.head))) {
            p.head.unbind
            p = p.tail
          }

          trail = _trail
      }
    else
      success = false
  }

  def execute {
    val inst = prog(pc)

    if (trace)
      println( pc, inst, dataStack )

    pc += 1

    inst match {
      case PushInst( d ) => push( d )
      case VarInst( n ) => push( frame.vars(n) )
      case StructureInst( f ) => pushStructure( f )
      case ElementInst( n ) => push( popValue.asInstanceOf[Product].productElement(n) )
      case ReturnInst =>
        pc = frame.ret
        frame = pop.asInstanceOf[Frame]
//      case BindInst( n ) =>
//        if (frame.vars(n).bound)
//          unify( frame.vars(n).eval, popValue )
//        else
//          frame.vars(n).bind( popValue )
      case FunctorInst( f ) =>
        top match {
          case c: Structure if c.functor == f =>
          case _ => fail
        }
      case DupInst => push( top )
      case EqInst => push( popValue == popValue )
      case BranchIfInst( disp ) =>
        if (popBoolean)
          pc += disp
      case FailInst => fail
      case ChoiceInst( disp ) => choiceStack push State( dataStack, pc + disp, frame, trail )
      case CallInst( entry ) => call( entry )
      case CallIndirectInst( pos, f@Functor(Symbol(name), arity) ) =>
        prog get f match {
          case None => pos.error( s"rule $name/$arity not defined" )
          case Some( p ) => call( p.entry )
        }
      case DropInst => pop
      case PushFrameInst => pushFrame
      case FrameInst( vars ) => frame = new Frame( vars, popInt )
      case PredicateInst( pred ) => pred( this )
      case UnifyInst => unify( popValue, popValue )
//        val const = popValue
//
//        popValue match {
//          case v: Variable => v bind const
//          case v => if (v != const) fail
//        }
    }
  }

  def unify( a: Any, b: Any ): Unit = {
    (a, b) match {
      case (a: Variable, _) => a bind b
      case (_, b: Variable) => b bind a
      case (Structure( Functor(n1, a1), args1 ), Structure( Functor(n2, a2), args2 )) =>
        if (n1 == n2 && a1 == a2)
          for (i <- 0 until a1)
            unify( args1(i), args2(i) )
        else
          fail
      case _ =>
        if (a != b)
          fail
    }
  }

  def run = {
    while (pc >= 0 && success) {
      execute
    }

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

    val num = Variable.count
    var binding: Any = _
    var bound = false

    def unbound = ! bound

    def bind( v: Any ): Unit = {
      binding = v
      bound = true
      trail ::= this
    }

    def unbind: Unit = {
      bound = false
    }

    def eval: Any =
      if (bound)
        binding match {
          case v: Variable if v eq this => sys.error( "self binding" )
          case v: Variable => v.eval
          case v => v
        }
      else
        this

    override def toString: String =
      eval match {
        case v: Variable => s"_V${v.num}"
        case v => v.toString
      }
  }

}