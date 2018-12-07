package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.Reader

import scala.collection.mutable


object Compiler {

  class Vars {
    val vars = new mutable.LinkedHashMap[String, Int]

    def count = vars.size

    def num( name: String ) = {
      vars get name match {
        case None =>
          vars(name) = count + 1
          count
        case Some( n ) => n
      }
    }

    def get( name: String ) = vars get name
  }

  def compile( ast: PrologAST )( implicit prog: Program ): Unit = {
    phase1( ast )
    phase2( prog )
  }

  def phase1( ast: PrologAST )( implicit prog: Program ) {
    ast match {
      case SourceAST( clauses ) => clauses foreach phase1
      case ClauseAST( clause@CompoundAST(r, ":-", List(CompoundAST(h, f, args), body)) ) =>
        prog.procedure( f, args.length ).clauses += clause
      case ClauseAST( clause@CompoundAST(r, ":-", List(AtomAST(h, name), body)) ) =>
        prog.procedure( name, 0 ).clauses += clause
      case ClauseAST( clause@CompoundAST(r, fact, args) ) =>
        prog.procedure( fact, args.length ).clauses += clause
//      case ClauseAST( clause@AtomAST(r, name) ) =>
//        prog.procedure( name, 0 ).clauses += clause
    }
  }

  def phase2( prog: Program ) {

    ast match {
      case SourceAST( clauses ) => clauses foreach phase1
      case ClauseAST( CompoundAST(r, ":-", List(CompoundAST(h, f, args), body)) ) =>
        val ptr = prog.pointer

        args foreach compileHead
        compileConjunct( body )
        prog += ReturnInst

        prog.procedure( f, args.length ).clauses += Clause( vars.count, code )
      case ClauseAST( CompoundAST(r, ":-", List(AtomAST(h, name), body)) ) =>
        val ptr = prog.pointer

        compileConjunct( body )
        prog += ReturnInst

        prog.procedure( name, 0 ).clauses += Clause( vars.count, code )
      case ClauseAST( CompoundAST(r, fact, args) ) =>
        val code = args.flatMap( compileTerm ) :+ ReturnInst

        prog.procedure( fact, args.length ).clauses += Clause( vars.count, code )
      case AtomAST( r, name ) =>
        prog.procedure( name, 0 ).clauses += Clause( vars.count, List(ReturnInst) )
    }
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
          prog += AtomMatchInst( Symbol(n) )
//        case VariableAST( _, n, _ ) =>
//          val VarDecl( idx, _, _ ) = variable( n, namespaces )
//
//          if (vars contains n)
//            code += MatchBindingInst( idx )
//          else {
//            vars += n
//            code += BindingInst
//          }
//        case NamedStructureAST( _, _, s ) =>
//          code += DupInst
//          code += BindingInst
//          compileHead( s, pos, namespaces )
        case CompoundAST( _, name, args ) =>
          prog += FunctorMatchInst( Functor(Symbol(name), args.length) )

          args.zipWithIndex foreach { case (e, i) =>
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
          prog += PushInst( IntegerData(n) )
          prog += EqInst
          prog += BranchIfInst( 1 )
          prog += FailInst
      }

    compileHead( term )
  }

  def compileTerm( term: TermAST )( implicit prog: Program, vars: Vars ): List[Instruction] =
    term match {
      case CompoundAST( pos, name, args ) =>
        args.flatMap( compileTerm ) :+ CompoundInst( Functor(Symbol(name), args.length) )
      case AtomAST( pos, name ) => List( PushInst(AtomData(Symbol(name))) )
      case WildcardAST( pos ) => Nil
      case VariableAST( pos, name ) => List( VarInst(vars.num(name)) )
      case IntegerAST( pos, v ) => List( PushInst(IntegerData(v)) )
      case FloatAST( pos, v ) => List( PushInst(FloatData(v)) )
    }

  def compileCall( ast: PrologAST )( implicit prog: Program, vars: Vars ): List[Instruction] =
    ast match {
//      case CompoundAST( pos, "is", List(VariableAST(r, name), expr) ) =>
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
      case CompoundAST( pos, "is", List(head, expr) ) => head.pos.error( s"variable was expected" )
      case CompoundAST( pos, name, args ) =>
        args.flatMap( compileTerm ) :+ CallInstruction( prog.procedure(name, args.length) )
      case AtomAST( pos, name ) => List( CallInstruction(prog.procedure(name, 0)) )
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

  def compileConjunct( ast: PrologAST )( implicit prog: Program, vars: Vars ): List[Instruction] =
    ast match {
      case CompoundAST( r, ",", List(head, tail) ) => compileCall( head ) ++ compileConjunct( tail )
      case t => compileCall( t )
    }

}