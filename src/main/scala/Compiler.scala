package xyz.hyperreal.prolog

import scala.collection.mutable


object Compiler {

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

  def phase1( ast: PrologAST, prog: Program ) {
    ast match {
      case SourceAST( clauses ) => clauses foreach (phase1( _, prog ))
      case ClauseAST( clause@CompoundAST(r, ":-", List(CompoundAST(h, f, args), body)) ) =>
        prog.procedure( f, args.length ).clauses += Clause( 0, clause )
      case ClauseAST( clause@CompoundAST(r, ":-", List(AtomAST(h, name), body)) ) =>
        prog.procedure( name, 0 ).clauses += Clause( 0, clause )
      case ClauseAST( clause@CompoundAST(r, fact, args) ) =>
        prog.procedure( fact, args.length ).clauses += Clause( 0, clause )
      case ClauseAST( clause@AtomAST(r, name) ) =>
        prog.procedure( name, 0 ).clauses += Clause( 0, clause )
    }
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

  def compileClause( ast: TermAST )( implicit prog: Program ) = {
    implicit val vars = new Vars

    ast match {
      case CompoundAST( r, ":-", List(CompoundAST(_, f, args), body) ) =>
        prog.patch( (_, _) => FrameInst(vars.count) ) {
          args.reverse foreach compileHead
          compileConjunct( body )
        }

        prog += ReturnInst
      case CompoundAST( r, ":-", List(AtomAST(_, _), body) ) =>
        prog += FrameInst( 0 )
        compileConjunct( body )
        prog += ReturnInst
      case CompoundAST( _, _, args ) =>
        prog.patch( (_, _) => FrameInst(vars.count) ) {
          args.reverse foreach compileHead
        }

        prog += ReturnInst
      case AtomAST( r, name ) =>
        prog += FrameInst( 0 )
        prog += ReturnInst
    }

    vars.count
  }

  private def compileHead( term: TermAST )( implicit prog: Program, vars: Vars ): Unit = {
    def compileHead( term: TermAST ): Unit =
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
//        case VariableStructureAST( _, "_", _ ) => code += DropInst
        case AtomAST( _, n ) =>
          prog += PushInst( Symbol(n) )
          prog += UnifyInst
        case WildcardAST( _ ) => prog += DropInst
        case VariableAST( _, name ) =>
          prog += VarInst( vars.num(name) )
          prog += UnifyInst
//          prog += BindInst( vars.num(n) )
//        case NamedStructureAST( _, _, s ) =>
//          code += DupInst
//          code += BindingInst
//          compileHead( s, pos, namespaces )
        case CompoundAST( _, name, args ) =>
          prog += FunctorInst( Functor(Symbol(name), args.length) )

          args.zipWithIndex.reverse foreach {
            case (e, i) =>
              if (i < args.length - 1)
                prog += DupInst

              prog += ElementInst( i )
              compileHead( e )
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
        case IntegerAST( _, n ) =>
          prog += PushInst( n )
          prog += UnifyInst
        case FloatAST( _, n ) =>
          prog += PushInst( n )
          prog += UnifyInst
      }

    compileHead( term )
  }

  def compileTerm( term: TermAST )( implicit prog: Program, vars: Vars ): Unit =
    term match {
      case CompoundAST( pos, name, args ) =>
        args foreach compileTerm
        prog += StructureInst( Functor(Symbol(name), args.length) )
      case AtomAST( pos, name ) =>
        prog += PushInst( Symbol(name) )
      case WildcardAST( pos ) => pos.error( "wildcard not allowed here" )
      case VariableAST( pos, name ) => prog += VarInst( vars.num(name) )
      case IntegerAST( pos, v ) => prog += PushInst( v )
      case FloatAST( pos, v ) => prog += PushInst( v )
    }

  def compileCall( ast: PrologAST )( implicit prog: Program, vars: Vars ): Unit =
    ast match {
      case CompoundAST( pos, "is", List(VariableAST(r, name), expr) ) =>
//        val exprvars = new mutable.HashSet[(Int, Int)]
//
//        def addvar( term: TermAST )( implicit vars: Vars ): Unit =
//          term match {
//            case v@VariableAST( r, name ) =>
//              vars get name match {
//                case None => r.error( s"variable 'name' does not occur previously in the clause" )
//                case Some( n ) =>
//                  v.name += '\''
//                  exprvars += (n -> vars.num( v.name ))
//              }
//            case CompoundAST( r, name, args ) => args foreach addvar
//            case _ =>
//          }
//
//        addvar( expr )
//
//        val buf = new ListBuffer[Instruction]
//
//        for ((v, v1) <- exprvars)
//          buf += EvalInstruction( v, v1 )
//
//        compileExpression( expr, buf )
//        buf += ResultInstruction( vars.num(name) )
//        buf.toList
      case CompoundAST( _, "is", List(head, _) ) => head.pos.error( s"variable was expected" )
      case CompoundAST( _, name, args ) if prog.defined( name, args.length ) =>
        prog += PushFrameInst
        args foreach compileTerm

        val f = functor( name, args.length )

        prog.procedure( f ).entry match {
          case -1 => prog.fixup( f )
          case entry => prog += CallInst( entry )
        }
      case CompoundAST( _, name, args ) if Builtins.predicates contains functor( name, args.length ) =>
        args foreach compileTerm
        prog += PredicateInst( Builtins.predicates(functor(name, args.length)) )
      case CompoundAST( pos, name, args ) =>
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
      case AtomAST( _, name ) if Builtins.predicates contains functor( name, 0 ) =>
        prog += PredicateInst( Builtins.predicates(functor( name, 0)) )
      case AtomAST( pos, name ) => pos.error( s"rule $name/0 not defined" )
    }

//  def compileExpression( expr: TermAST, buf: ListBuffer[Instruction] )( implicit vars: Vars ): Unit =
//    expr match {
//      case x: NumericAST => buf += PushNumInstruction( x.n )
//      case VariableAST( pos, name ) => buf += PushVarInstruction( vars.num(name) )
//      case CompoundAST( pos, op@("+"|"-"), List(left, right) ) =>
//        compileExpression( left, buf )
//        compileExpression( right, buf )
//        buf +=
//          (op match {
//            case "+" => AddInstruction
//            case "-" => SubInstruction
//          })
//    }

  def compileConjunct( ast: PrologAST )( implicit prog: Program, vars: Vars ): Unit =
    ast match {
      case CompoundAST( r, ",", List(head, tail) ) =>
        compileCall( head )
        compileConjunct( tail )
      case t => compileCall( t )
    }

}