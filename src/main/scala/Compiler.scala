package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.Reader

import scala.collection.mutable


object Compiler {

  var debug = false

  class Vars {
    val vars = new mutable.LinkedHashMap[String, Int]

    def count = vars.size

    def num( name: String ) = {
      vars get name match {
        case None =>
          val n = count

          vars(name) = n
          n
        case Some( n ) => n
      }
    }

    def get( name: String ) = vars get name
  }

  def compile( ast: PrologAST, prog: Program ): Unit = {
    phase1( ast, prog )
    phase2( prog )
  }

  def phase1( ast: PrologAST, prog: Program ): Unit =
    ast match {
      case SourceAST( clauses ) => clauses foreach (phase1( _, prog ))
      case ClauseAST( clause@StructureAST(r, ":-", List(StructureAST(h, f, args), body)) ) =>
        prog.procedure( f, args.length ).clauses += Clause( 0, clause )
      case ClauseAST( clause@StructureAST(r, ":-", List(AtomAST(h, name), body)) ) =>
        prog.procedure( name, 0 ).clauses += Clause( 0, clause )
      case ClauseAST( clause@StructureAST(r, fact, args) ) =>
        prog.procedure( fact, args.length ).clauses += Clause( 0, clause )
      case ClauseAST( clause@AtomAST(r, name) ) =>
        prog.procedure( name, 0 ).clauses += Clause( 0, clause )
    }

  def phase2( implicit prog: Program ) {
    prog.procedures foreach {
      case proc@Procedure( _, _, _, clauses ) =>
        proc.entry = prog.pointer

        for (c <- clauses.init) {
          prog.patch( (ptr, len) => ChoiceInst(len - ptr - 1) ) {
            c.vars = compileClause( c.ast )
          }
        }

        clauses.last.vars = compileClause( clauses.last.ast )
        proc.end = prog.pointer
    }

    for ((addr, f) <- prog.fixups)
      prog(addr) = CallInst( prog.procedure(f).entry )
  }

  def debugInst( msg: String, pos: Reader )( implicit prog: Program ) =
    if (debug)
      prog += DebugInst( msg, pos )

  def compileClause( ast: TermAST )( implicit prog: Program ) = {
    implicit val vars = new Vars

    ast match {
      case StructureAST( r, ":-", List(StructureAST(pos, f, args), body) ) =>
        debugInst( s"rule $f/${args.length}", pos )
        prog.patch( (_, _) => FrameInst(vars.count) ) {
          args.reverse foreach compileHead
          compileBody( body )
        }

        prog += ReturnInst
      case StructureAST( r, ":-", List(AtomAST(pos, n), body) ) =>
        debugInst( s"rule $n/0", pos )
        prog += FrameInst( 0 )
        compileBody( body )
        prog += ReturnInst
      case StructureAST( r, f, args ) =>
        debugInst( s"fact $f/${args.length}", r )
        prog.patch( (_, _) => FrameInst(vars.count) ) {
          args.reverse foreach compileHead
        }

        prog += ReturnInst
      case AtomAST( r, name ) =>
        debugInst( s"fact $name/0", r )
        prog += FrameInst( 0 )
        prog += ReturnInst
    }

    vars.count
  }

  def compileHead( term: TermAST )( implicit prog: Program, vars: Vars ): Unit =
    term match {
//        case TupleStructureAST( _, args ) =>
//          code += TypeCheckInst( struc, pos )
//
//          args.zipWithIndex foreach {
//            case (e, i) =>
//              if (i < args.length - 1)
//                code += DupInst
//
//              code += TupleElementInst( i )
//              compileHead( e, pos, namespaces )
//          }
//        case NilStructureAST =>
//          code += TypeCheckInst( term, pos )
//          code += DropInst
//        case ListStructureAST( _, l ) =>
//          code += TypeCheckInst( term, pos )
//
//          l foreach { e =>
//            code += DupInst
//            code += EmptyInst
//            code += BranchIfNotInst( 1 )
//            code += FailInst
//            code += DupInst
//            code += ListHeadInst
//            compileHead( e, pos, namespaces )
//            code += ListTailInst
//          }
//
//          code += EmptyInst
//          code += BranchIfInst( 1 )
//          code += FailInst
//        case ConsStructureAST( _, head, tail ) =>
//          code += TypeCheckInst( term, pos )
//          code += DupInst
//          code += ListHeadInst
//          compileHead( head, pos, namespaces )
//          code += ListTailInst
//          compileHead( tail, pos, namespaces )
      case AtomAST( _, n ) =>
        prog += PushInst( Symbol(n) )
        prog += UnifyInst
      case WildcardAST( _ ) => prog += DropInst
      case VariableAST( _, name ) => prog += VarUnifyInst( vars.num(name) )
      case StructureAST( _, name, args ) =>
        prog += FunctorInst( Functor(Symbol(name), args.length) )

        args.zipWithIndex foreach {
          case (e, i) =>
            if (i < args.length - 1)
              prog += DupInst

            compileTerm( e )
            prog += ElementUnifyInst( i )
        }
//        case AlternationStructureAST( l ) =>
//          val jumps = new ArrayBuffer[Int]
//
//          for (s <- l.init) {
//            val backptr = code.length
//
//            code += null
//            compileHead( s, pos, namespaces )
//            jumps += code.length
//            code += null
//            code(backptr) = ChoiceInst( code.length - backptr - 1 )
//          }
//
//          compileHead( l.last, pos, namespaces )
//
//          for (b <- jumps)
//            code(b) = BranchInst( code.length - b - 1 )
      case n: NumericAST =>
        prog += PushInst( n.v )
        prog += UnifyInst
    }

  def ground( term: TermAST ): Boolean =
    term match {
      case StructureAST( _, _, args ) => args forall ground
      case AtomAST( _, _ ) | _: NumericAST => true
      case WildcardAST( _ ) | VariableAST( _, _ ) => false
    }

  def constant( term: TermAST ): Any =
    term match {
      case StructureAST( _, name, args ) => Structure( functor(name, args.length), args map constant toArray )
      case AtomAST( _, name ) => Symbol( name )
      case n: NumericAST => n.v
    }

  def compileTerm( term: TermAST )( implicit prog: Program, vars: Vars ): Unit =
    term match {
      case s: StructureAST if ground( s ) => prog += PushInst( constant(s) )
      case StructureAST( _, name, args ) =>
        args foreach compileTerm
        prog += StructureInst( functor(name, args.length) )
      case AtomAST( pos, name ) => prog += PushInst( Symbol(name) )
      case WildcardAST( pos ) => prog += PushInst( WILDCARD )
      case VariableAST( _, name ) => prog += VarInst( vars.num(name) )
      case n: NumericAST => prog += PushInst( n.v )
    }

  def compileCall( ast: PrologAST )( implicit prog: Program, vars: Vars ): Unit =
    ast match {
      case StructureAST( pos, "=", List(VariableAST(_, lname), right) ) =>
        compileTerm( right )
        prog += VarUnifyInst( vars.num(lname) )
      case StructureAST( pos, "=", List(left, VariableAST(_, rname)) ) =>
        compileTerm( left )
        prog += VarUnifyInst( vars.num(rname) )
      case StructureAST( pos, "=", List(left, right) ) =>
        compileTerm( left )
        compileTerm( right )
        prog += UnifyInst
      case StructureAST( pos, "is", List(VariableAST(_, rname), expr) ) =>
        compileArithmetic( expr )
        prog += VarInst( vars.num(rname) )
        prog += UnifyInst
      case StructureAST( _, "is", List(head, _) ) => head.pos.error( s"variable was expected" )
      case StructureAST( pos, "=:=", List(left, right) ) =>
        compileArithmetic( left )
        compileArithmetic( right )
        prog += EqInst
      case StructureAST( pos, "=\\=", List(left, right) ) =>
        compileArithmetic( left )
        compileArithmetic( right )
        prog += NeInst
      case StructureAST( pos, "<", List(left, right) ) =>
        compileArithmetic( left )
        compileArithmetic( right )
        prog += LtInst
      case StructureAST( pos, "=<", List(left, right) ) =>
        compileArithmetic( left )
        compileArithmetic( right )
        prog += LeInst
      case StructureAST( pos, ">", List(left, right) ) =>
        compileArithmetic( left )
        compileArithmetic( right )
        prog += GtInst
      case StructureAST( pos, ">=", List(left, right) ) =>
        compileArithmetic( left )
        compileArithmetic( right )
        prog += GeInst
      case StructureAST( _, name, args ) if prog.defined( name, args.length ) =>
        prog += PushFrameInst
        args foreach compileTerm

        val f = functor( name, args.length )

        prog.procedure( f ).entry match {
          case -1 => prog.fixup( f )
          case entry => prog += CallInst( entry )
        }
      case StructureAST( _, name, args ) if Builtin exists functor( name, args.length ) =>
        args foreach compileTerm
        prog += NativeInst( Builtin.predicate(functor(name, args.length)) )
      case StructureAST( pos, name, args ) =>
        prog += PushFrameInst
        args foreach compileTerm
        prog += CallIndirectInst( pos, functor(name, args.length) )
      case AtomAST( _, name ) if prog.defined( name, 0 ) =>
        prog += PushFrameInst

        val f = functor( name, 0 )

        prog.procedure( f ).entry match {
          case -1 => prog.fixup( f )
          case entry => prog += CallInst( entry )
        }
      case AtomAST( _, name ) if Builtin exists functor( name, 0 ) =>
        prog += NativeInst( Builtin.predicate(functor( name, 0)) )
      case AtomAST( pos, name ) =>
        prog += PushFrameInst
        prog += CallIndirectInst( pos, functor(name, 0) )
    }

  def compileArithmetic( expr: TermAST )( implicit prog: Program, vars: Vars ) {
    val exprvars = new mutable.HashSet[(Reader, String, Int, Int)]

    def addvar( term: TermAST )( implicit vars: Vars ): Unit =
      term match {
        case v@VariableAST( r, name ) =>
          vars get name match {
            case None => r.error( s"variable '$name' does not occur previously in the clause" )
            case Some( n ) =>
              v.name += '\''
              exprvars += ((r, name, n, vars.num( v.name )))
          }
        case StructureAST( _, _, args ) => args foreach addvar
        case _ =>
      }

    addvar( expr )

    for ((r, n, v, v1) <- exprvars)
      prog += EvalInst( r, n, v, v1 )

    compileExpression( expr )
  }

  def compileExpression( expr: TermAST )( implicit prog: Program, vars: Vars ): Unit =
    expr match {
      case x: NumericAST => prog += PushInst( x.v )
      case VariableAST( pos, name ) => prog += VarInst( vars.num(name) )
      case StructureAST( pos, op@("+"|"-"), List(left, right) ) =>
        compileExpression( left )
        compileExpression( right )
        prog +=
          (op match {
            case "+" => AddInst
            case "-" => SubInst
          })
      case StructureAST( pos, op@("-"), List(arg) ) =>
        compileExpression( arg )
        prog +=
          (op match {
            case "-" => NegInst
          })
      case StructureAST( _, name, args ) if Math exists functor( name, args.length ) =>
        args foreach compileTerm
        prog += NativeInst( Math.function(functor(name, args.length)) )
      case StructureAST( pos, name, args ) => pos.error( s"function $name/${args.length} not found" )
      case AtomAST( _, name ) if Math exists functor( name, 0 ) =>
        prog += NativeInst( Math.function(functor( name, 0)) )
      case AtomAST( pos, name ) => pos.error( s"constant '$name' not found" )
    }

  def compileBody( ast: PrologAST )(implicit prog: Program, vars: Vars ): Unit =
    ast match {
      case StructureAST( r, ",", List(head, tail) ) =>
        compileBody( head )
        compileBody( tail )
      case t => compileCall( t )
    }

}