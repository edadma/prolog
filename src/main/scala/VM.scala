package xyz.hyperreal.prolog

import scala.collection.mutable
import scala.collection.mutable.ArrayStack
import xyz.hyperreal.lia


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

  def interpall( goal: TermAST ) = {
    val resultset = new mutable.HashSet[Map[String, Any]]

    interp( goal ) match {
      case Some( r ) =>
        def results( res: Map[String, Any] ): Unit = {
          resultset += res
          fail

          run match {
            case Some( r1 ) => results( r1 )
            case None =>
          }
        }

        results( r )
      case None =>
    }

    resultset.toSet
  }

  def interp( goal: TermAST ) = {
    success = true
    vars = new VarMap
    trail = Nil

    goal match {
      case StructureAST( _, name, args ) if prog.defined( name, args.length ) =>
        pushFrame
        args foreach interpTerm
        call( prog.procedure(name, args.length).entry )
        run
      case StructureAST( _, name, args ) if Builtin exists functor( name, args.length ) =>
        args foreach interpTerm
        Builtin.predicate(functor(name, args.length))( this )
        Some( vars.map )
      case StructureAST( pos, name, args ) => pos.error( s"rule $name/${args.length} not defined" )
      case AtomAST( _, name ) if prog.defined( name, 0 ) =>
        pushFrame
        call( prog.procedure( name, 0).entry )
        run
      case AtomAST( _, name ) if Builtin exists functor( name, 0 ) =>
        Builtin.predicate(functor(name, 0))( this )
        Some( vars.map )
      case AtomAST( pos, name ) => pos.error( s"rule $name/0 not defined" )
      case _ => goal.pos.error( "expected a rule" )
    }
  }

  def interpTerm( term: TermAST )( implicit vars: VarMap ): Unit =
    term match {
      case StructureAST( pos, name, args ) =>
        args foreach interpTerm
        pushStructure( Functor(Symbol(name), args.length) )
      case AtomAST( pos, name ) => push( Symbol(name) )
      case WildcardAST( pos ) => push( Wildcard )
      case VariableAST( pos, name ) => push( vars(name).eval )
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

  def popValue = pop
//    pop match {
//      case v: Variable => v.eval
//      case v => v
//    }

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
      case VarInst( n ) => push( frame.vars(n).eval )
      case VarUnifyInst( n ) => unify( frame.vars(n).eval, popValue )
      case StructureInst( f ) => pushStructure( f )
      case ElementInst( n ) => push( popValue.asInstanceOf[Product].productElement(n) )
      case ReturnInst =>
        pc = frame.ret
        frame = pop.asInstanceOf[Frame]
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
      case NativeInst( func ) => func( this )
      case UnifyInst => unify( popValue, popValue )
      case EvalInst( pos, name, v1, v2 ) =>
        frame.vars(v1).eval match {
          case _: Variable => pos.error( s"variable '$name' is unbound" )
          case t => unify( eval(t), frame.vars(v2) )
        }
      case AddInst => push( lia.Math('+, popValue, popValue) )
      case SubInst =>
        val r = popValue

        push( lia.Math('-, popValue, r) )
      case NegInst => push( lia.Math('-, popValue) )
    }
  }

  def eval( term: Any ): Number =
    term match {
      case n: Number => n
      case Structure( Functor(Symbol(operator@("+"|"-")), arity), Vector(left, right) ) =>
        val l = eval( left )
        val r = eval( right )

        operator match {
          case "+" => lia.Math( '+, l, r ).asInstanceOf[Number]
        }
    }

  def unify( a: Any, b: Any ): Unit = {
    (a, b) match {
      case (Wildcard, _) | (_, Wildcard) =>
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
    while (pc >= 0 && success)
      execute

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
    val num = Variable.count

    Variable.count += 1

    var binding: Any = _
    var bound = false

    def unbound = !bound

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
          case v: Variable => v.eval
          case v => v
        }
      else
        this

    override def toString: String =
      eval match {
        case v: Variable => s"#${v.num}"
        case v => v.toString
      }
  }

//  object Wildcard extends Variable {
//    override def bind( v: Any ) {}
//    override def unbind {}
//    override def eval = this
//    override def toString = "_"
//  }

}