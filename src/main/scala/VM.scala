package xyz.hyperreal.prolog

import scala.collection.mutable
import xyz.hyperreal.lia

import scala.collection.immutable.SortedMap


object VM {

  val FM_EQ = lia.Math.lookup( '== )
  val FM_NE = lia.Math.lookup( '!= )
  val FM_LT = lia.Math.lookup( '< )
  val FM_LE = lia.Math.lookup( '<= )
  val FM_GT = lia.Math.lookup( '> )
  val FM_GE = lia.Math.lookup( '>= )

  val FM_ADD = lia.Math.lookup( '+ )
  val FM_SUB = lia.Math.lookup( '- )
  val FM_MUL = lia.Math.lookup( '* )
  val FM_DIV = lia.Math.lookup( '/ )
  val FM_MOD = lia.Math.lookup( 'mod )

}

class VM( val prog: Program ) {

  import VM._

  var trace = false
  var debug = false
  var out = Console.out

  var success = true
  var trail: List[Variable] = Nil
//  implicit var vars: VarMap = _
//
//  class VarMap {
//    val vars = new mutable.HashMap[String, Variable]
//
//    def apply( name: String ) =
//      vars get name match {
//        case None =>
//          val v = new Variable
//
//          vars(name) = v
//          v
//        case Some( v ) => v
//      }
//
//    def map = vars map { case (k, v) => (k, v.eval) } toMap
//  }

  case class State( dataStack: List[Any], pb: Block, pc: Int, frame: Frame, trail: List[Variable], mark: List[State],
                    cut: List[State] ) {
    override def toString: String = s"[state pb=$pb pc=$pc]"
  }

  class Frame( size: Int, val retpc: Int, val retpb: Block ) {
    val vars = Array.fill[Variable]( size )( new Variable )

    override def toString: String = s"[frame size=$size retpb=$retpb retpc=$retpc]"
  }

  var choiceStack: List[State] = Nil
  var cut: List[State] = _
  var mark: List[State] = _
  var dataStack: List[Any] = Nil
  var pb: Block = _
  var pc = -1
  var frame: Frame = new Frame( 0, -1, null )

//  def interpall( goal: TermAST ) = {
//    val resultset = new mutable.HashSet[Map[String, Any]]
//
//    interp( goal ) match {
//      case Some( r ) =>
//        def results( res: Map[String, Any] ): Unit = {
//          val res1 = res map { case (k, v) => k -> copy( v ) }
//
//          if (trace || debug)
//            out.println( s"==> $res1" )
//
//          resultset += res1
//
//          if (fail)
//            run match {
//              case Some( r1 ) => results( r1 )
//              case None =>
//            }
//        }
//
//        results( r )
//      case None =>
//    }
//
//    resultset.toSet
//  }
//
//  def interp( goal: TermAST ) = {
//    success = true
//    vars = new VarMap
//    trail = Nil
//    cut = Nil
//
//    goal match {
//      case StructureAST( _, name, args ) if prog.defined( name, args.length ) =>
//        pushFrame
//        args foreach interpTerm
//        call( prog.procedure(name, args.length).block, prog.procedure(name, args.length).entry )
//        run
//      case StructureAST( _, name, args ) if Builtin exists functor( name, args.length ) =>
//        args foreach interpTerm
//        Builtin.predicate(functor(name, args.length))( this )
//        Some( vars.map )
//      case StructureAST( pos, name, args ) => pos.error( s"rule $name/${args.length} not defined" )
//      case AtomAST( _, name ) if prog.defined( name, 0 ) =>
//        pushFrame
//        call( prog.procedure( name, 0).block, prog.procedure( name, 0).entry )
//        run
//      case AtomAST( _, name ) if Builtin exists functor( name, 0 ) =>
//        Builtin.predicate(functor(name, 0))( this )
//        Some( vars.map )
//      case AtomAST( pos, name ) => pos.error( s"rule $name/0 not defined" )
//      case _ => goal.pos.error( "expected a rule" )
//    }
//  }
//
//  def interpTerm( term: TermAST )( implicit vars: VarMap ): Unit =
//    term match {
//      case StructureAST( pos, name, args ) =>
//        args foreach interpTerm
//        pushStructure( Functor(Symbol(name), args.length) )
//      case AtomAST( pos, name ) => push( Symbol(name) )
//      case AnonymousAST( pos ) => push( new Variable )
//      case VariableAST( pos, name ) => push( vars(name).eval )
//      case n: NumericAST => push( n.v )
//    }

  def data( term: TermAST, vars: mutable.HashMap[String, Variable] = new mutable.HashMap ): Any =
    term match {
      case StructureAST( _, name, args ) => Structure( functor(name, args.length), args map (data(_, vars)) toArray )
      case AtomAST( _, name ) => Symbol( name )
      case AnonymousAST( _ ) => new Variable( "_" )
      case VariableAST( _, name ) =>
        vars get name match {
          case None =>
            val v = new Variable( name )

            vars(name) = v
            v
          case Some( v ) => v
        }
      case n: NumericAST => n.v
    }

  def pushFrame = push( frame )

  def pushStructure( f: Functor ): Unit = {
    val args = new Array[Any]( f.arity )

    for (i <- f.arity - 1 to 0 by -1)
      args(i) = pop

    push( Structure(f, args) )
  }

  def top =
    if (dataStack nonEmpty)
      vareval( dataStack.head )
    else
      sys.error( "data stack underflow" )

  def pop = {
    val res = top

    dataStack = dataStack.tail
    res
  }

  def popInt = pop.asInstanceOf[Int]

  def popBoolean = pop.asInstanceOf[Boolean]

  def push( d: Any ): Unit = dataStack ::= d

  def call( block: Block, entry: Int ): Unit = {
    push( pb )
    push( pc )
    pb = block
    pc = entry
  }

  def choice( disp: Int ) = choiceStack ::= State( dataStack, pb, pc + disp, frame, trail, mark, cut )

  def compare( term1: Any, term2: Any ): Option[Int] =
    (vareval( term1 ), vareval( term2 )) match {
      case (v1: Variable, v2: Variable) => Some( v2.num - v1.num )
      case (Structure( f1, args1 ), Structure( f2, args2 )) =>
        functorOrdering.compare( f1, f2 ) match {
          case 0 =>
            for ((a1, a2) <- args1 zip args2)
              compare( a1, a2 ) match {
                case Some( 0 ) =>
                case c => return c
              }

            Some( 0 )
          case c => Some( c )
        }
      case (Symbol( s1 ), Symbol( s2 )) => Some( s1 compareTo s2 )
      case (a1: Comparable[_], a2: Comparable[_]) if a1.getClass == a2.getClass => Some( a1.asInstanceOf[Comparable[Any]] compareTo a2 )
      case _ => None
    }

  def execute {
    val inst = pb(pc)

    if (trace)
      out.println( pc, instruction(inst), dataStack )

    pc += 1

    inst match {
      case TermEqInst =>
        if (copy( pop ) != copy( pop ))
          fail
      case TermLtInst =>
        compare( copy( pop ), copy( pop ) ) match {
          case Some( c ) if c <= 0 => fail
          case None => fail
          case _ =>
        }
      case TermLeInst =>
        compare( copy( pop ), copy( pop ) ) match {
          case Some( c ) if c < 0 => fail
          case None => fail
          case _ =>
        }
      case VarInst =>
        if (!pop.isInstanceOf[Variable])
          fail
      case NonvarInst =>
        if (pop.isInstanceOf[Variable])
          fail
      case DebugInst( _, _ ) if !debug =>
      case DebugInst( msg, null ) => out.println( msg )
      case DebugInst( msg, pos ) => out.println( pos.longErrorText(msg) )
      case PushInst( d ) => push( d )
      case PushVarInst( n ) => push( frame.vars(n) )
      case VarUnifyInst( n ) =>
        val v = frame.vars(n).eval
        val p = pop

        if (debug)
          out.println( s"var: $v   pop: $p" )

        unify( v, p )
      case StructureInst( f ) => pushStructure( f )
      case ElementUnifyInst( n ) =>
        val v = pop
        val s = pop.asInstanceOf[Compound]

        vareval( s productElement n ) match {
          case EMPTY => s.update( n, v )
          case u: Variable =>
            unify( u, v )
            s.update( n, u.eval )
          case e => unify( e, v )
        }
      case ReturnInst =>
        pc = frame.retpc
        pb = frame.retpb
        frame = pop.asInstanceOf[Frame]
      case FunctorInst( f ) =>
        top match {
          case v: Variable =>
            val s = Structure( f, Array.fill(f.arity)(EMPTY) )

            v bind s
            pop
            push( s )
          case c: Structure if c.functor == f =>
          case _ => fail
        }
      case DupInst => push( top )
      case EqInst => if (!lia.Math.predicate( FM_EQ, pop, pop )) fail
      case NeInst => if (!lia.Math.predicate( FM_NE, pop, pop )) fail
      case GtInst => if (!lia.Math.predicate( FM_GT, pop, pop )) fail
      case GeInst => if (!lia.Math.predicate( FM_GE, pop, pop )) fail
      case LtInst => if (!lia.Math.predicate( FM_LT, pop, pop )) fail
      case LeInst => if (!lia.Math.predicate( FM_LE, pop, pop )) fail
      case BranchIfInst( disp ) =>
        if (popBoolean)
          pc += disp
      case BranchInst( disp ) => pc += disp
      case FailInst => fail
      case ChoiceInst( disp ) => choice( disp )
      case CutChoiceInst( disp ) =>
        cut = choiceStack
        choice( disp )
      case CutInst =>
        mark = null
        choiceStack = cut
      case MarkInst( disp ) =>
        mark = choiceStack
        choice( disp )
      case UnmarkInst =>
        if (mark ne null)
          choiceStack = mark
      case CallBlockInst => call( pop.asInstanceOf[Block], 0 )
      case CallProcedureInst( p ) => call( p.block, p.entry )
      case CallIndirectInst( pos, f@Functor(Symbol(name), arity) ) =>
        prog get f match {
          case None => problem( pos, s"rule $name/$arity not defined" )
          case Some( p ) => call( p.block, p.entry )
        }
      case DropInst => pop
      case PushFrameInst => pushFrame
      case FrameInst( vars ) => frame = new Frame( vars, popInt, pop.asInstanceOf[Block] )
      case NativeInst( func, _, _ ) => func( this )
      case UnifyInst => unify( pop, pop )
      case EvalInst( pos, name, v ) =>
        frame.vars(v).eval match {
          case _: Variable => problem( pos, s"variable '$name' is unbound" )
          case t => push( eval(t) )
        }
      case AddInst => push( lia.Math(FM_ADD, pop, pop) )
      case SubInst =>
        val r = pop

        push( lia.Math(FM_SUB, pop, r) )
      case MulInst => push( lia.Math(FM_MUL, pop, pop) )
      case DivInst =>
        val r = pop

        push( lia.Math(FM_DIV, pop, r) )
      case ModInst =>
        val r = pop

        push( lia.Math(FM_MOD, pop, r) )
      case NegInst => push( lia.Math('-, pop) )
    }
  }

  def eval( term: Any ): Number =
    term match {
      case n: Number => n
      case Structure( Functor(Symbol(operator@("+"|"-")), _), Array(left, right) ) =>
        val l = eval( left )
        val r = eval( right )

        operator match {
          case "+" => lia.Math( FM_ADD, l, r ).asInstanceOf[Number]
          case "-" => lia.Math( FM_SUB, l, r ).asInstanceOf[Number]
        }
      case Structure( Functor(Symbol("-"), _), Array(expr) ) => lia.Math( '-, eval(expr) ).asInstanceOf[Number]
      case Structure( f, args ) if Math exists f => Math.function( f ).call( args map eval )
      case Structure( name, args ) => sys.error( s"function $name/${args.length} not found" )
      case s@Symbol( name ) if Math exists functor( name, 0 ) => Math.function( Functor(s, 0) ).call( Array() )
      case Symbol( name ) => sys.error( s"constant or system value '$name' not found" )
    }

  def fail = {
    if (trace || debug)
      out.println( "*** fail ***" )

    if (choiceStack nonEmpty) {
      val State( _dataStack, _pb, _pc, _frame, _trail, _mark, _cut ) = choiceStack.head

      choiceStack = choiceStack.tail
      dataStack = _dataStack
      pb = _pb
      pc = _pc
      frame = _frame
      mark = _mark
      cut = _cut

      var p = trail

      while (p.nonEmpty && (_trail.isEmpty || (p.head ne _trail.head))) {
        p.head.unbind
        p = p.tail
      }

      trail = _trail
      true
    } else {
      success = false
      false
    }
  }

  def copy( a: Any ): Any =
    vareval( a ) match {
      case s: Structure if groundTerm( s ) => s
      case Structure( functor, args ) => Structure( functor, args map copy )
      case v: Variable => new Variable( v.name )
      case a1 => a1
    }

  def unify( a: Any, b: Any ): Boolean =
    (vareval( a ), vareval( b )) match {
      case (a1: Variable, b1) =>
        a1 bind b1
        true
      case (a1, b1: Variable) =>
        b1 bind a1
        true
      case (Structure( Functor(n1, a1), args1 ), Structure( Functor(n2, a2), args2 )) =>
        if (n1 == n2 && a1 == a2)
          0 until a1 forall (i => unify( args1(i), args2(i) ))
        else {
          fail
          false
        }
      case (a1, b1) if a1 == b1 => true
      case _ =>
        fail
        false
    }

  def run( block: Block )( implicit vars: Vars ) = {
    while (pc < pb.length && pc >= 0 && success)
      execute

    if (trace)
      out.println( dataStack )

    if (success)
      Some( SortedMap( vars.varMap map { case (k, v) => (k, copy( frame.vars(v) )) }
        filterNot {case (k, _) => vars.evalSet(k) || k.startsWith("$")} toList: _* ) )
    else
      None
  }

  def runfirst( block: Block )( implicit vars: Vars ) = {
    choiceStack = Nil
    dataStack = Nil
    mark = null
    cut = Nil
    frame = new Frame( vars.count, -1, null )
    pb = block
    pc = 0
    success = true
    run( block )
  }

  def runall( block: Block )( implicit vars: Vars ) = {
    val resultset = new mutable.LinkedHashSet[Map[String, Any]]

    runfirst( block ) match {
      case Some( r ) =>
        def results( res: Map[String, Any] ): Unit = {
          if (trace || debug)
            out.println( s"==> $res" )

          resultset += res

          if (fail)
            run( block ) match {
              case Some( r1 ) => results( r1 )
              case None =>
            }
        }

        results( r )
      case None =>
    }

    resultset.toList
  }
  //  def run = {
//    while (pc >= 0 && success)
//      execute
//
//    if (trace)
//      out.println( dataStack )
//
//    if (success)
//      Some( vars.map )
//    else
//      None
//  }

  object Variable {
    private var count = 0
  }

  class Variable( val name: String = null ) {
    val num = Variable.count

    Variable.count += 1

    var binding: Any = _
    var bound = false

    def unbound = !bound

    def bind( v: Any ): Unit = {
      val last: Variable = end

      last.binding = v
      last.bound = true
      trail ::= last
    }

    def unbind: Unit = {
      binding = null
      bound = false
    }

    def end: Variable =
      if (bound)
        binding match {
          case v: Variable => v.end
          case _ => this
        }
      else
        this

    def eval: Any = {
      val last: Variable = end

      if (last.bound)
        last.binding
      else
        last
    }

    override def toString: String =
      eval match {
        case v: Variable if name ne null => s"#${v.name}"
        case v: Variable => s"#${v.num}"
        case v => v.toString
      }
  }

}