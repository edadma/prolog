package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.Reader

import scala.collection.mutable


object Compiler {

  var debug = false

  val reserved =
    Set(
      functor( "true", 0 ),
      functor( "fail", 0 ),
      functor( "false", 0 ),
      functor( "repeat", 0 ),
      functor( "is", 2 )
    )

  class Vars {
    val vars = new mutable.HashMap[String, Int]
    val evals = new mutable.HashSet[String]

    def count = vars.size

    def anon = num( '$' + count.toString )

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

    def eval( name: String ) =
      if (evals( name ))
        false
      else {
        evals += name
        true
      }
  }

  def compile( ast: PrologAST, prog: Program ): Unit = {
    phase1( ast, prog )
    phase2( prog )
  }

  def phase1( ast: PrologAST, prog: Program ): Unit =
    ast match {
      case SourceAST( clauses ) => clauses foreach (phase1( _, prog ))
      case ClauseAST( clause@StructureAST(r, ":-", List(StructureAST(h, name, args), body)) ) =>
        val f = functor( name, args.length )

        if (Builtin.exists( f ) || Math.exists( f ) || reserved(f))
          r.error( s"builtin procedure '$f' can't be redefined" )

        prog.procedure( f ).clauses += Clause( 0, clause )
      case ClauseAST( clause@StructureAST(r, ":-", List(AtomAST(h, name), body)) ) =>
        val f = functor( name, 0 )

        if (Builtin.exists( f ) || Math.exists( f ) || reserved(f))
          r.error( s"builtin procedure '$f' can't be redefined" )

        prog.procedure( f ).clauses += Clause( 0, clause )
      case ClauseAST( clause@StructureAST(r, name, args) ) =>
        val f = functor( name, args.length )

        if (Builtin.exists( f ) || Math.exists( f ) || reserved(f))
          r.error( s"builtin procedure '$f' can't be redefined" )

        prog.procedure( f ).clauses += Clause( 0, clause )
      case ClauseAST( clause@AtomAST(r, name) ) =>
        val f = functor( name, 0 )

        if (Builtin.exists( f ) || Math.exists( f ) || reserved(f))
          r.error( s"builtin procedure '$f' can't be redefined" )

        prog.procedure( f ).clauses += Clause( 0, clause )
    }

  def phase2( implicit prog: Program ) {
    prog.procedures foreach {
      case proc@Procedure( f, _, _, _, clauses ) =>
        proc.block = prog.block( f.toString )
        proc.entry = prog.pointer

        for (c <- clauses.init)
          prog.patch( (ptr, len) => CutChoiceInst(len - ptr - 1) ) {
            c.vars = compileClause( c.ast )
          }

        clauses.last.vars = compileClause( clauses.last.ast )
        proc.end = prog.pointer
    }

    for ((block, addr, f) <- prog.fixups) {
      val p = prog.procedure(f)

      block(addr) = CallInst( p.block, p.entry )
    }
  }

  def dbg( msg: String, pos: Reader )(implicit prog: Program ) =
    if (debug)
      prog += DebugInst( msg, pos )

  def compileClause( ast: TermAST )( implicit prog: Program ) = {
    implicit val vars = new Vars

    ast match {
      case StructureAST( r, ":-", List(StructureAST(pos, f, args), body) ) =>
        dbg( s"rule $f/${args.length}", pos )
        prog.patch( (_, _) => FrameInst(vars.count) ) {
          args.reverse foreach compileHead
          compileBody( body ) }
        prog += ReturnInst
      case StructureAST( r, ":-", List(AtomAST(pos, n), body) ) =>
        dbg( s"rule $n/0", pos )
        prog.patch( (_, _) => FrameInst(vars.count) ) {
          compileBody( body ) }
        prog += ReturnInst
      case StructureAST( r, f, args ) =>
        dbg( s"fact $f/${args.length}", r )
        prog.patch( (_, _) => FrameInst(vars.count) ) {
          args.reverse foreach compileHead }
        prog += ReturnInst
      case AtomAST( r, name ) =>
        dbg( s"fact $name/0", r )
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
      case AtomAST( r, n ) =>
        dbg( s"get atom $n", r )
        prog += PushInst( Symbol(n) )
        prog += UnifyInst
      case AnonymousAST( r ) =>
        dbg( "get anonymous", r )
        prog += DropInst
      case VariableAST( r, name ) =>
        dbg( s"get variable $name", r )
        prog += VarUnifyInst( vars.num(name) )
      case StructureAST( r, name, args ) =>
        dbg( s"get structure $name/${args.length}", r )
        prog += FunctorInst( Functor(Symbol(name), args.length) )

        args.zipWithIndex foreach {
          case (e, i) =>
            dbg( s"get arg $i", e.pos )

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
        dbg( s"get number ${n.v}", n.pos )
        prog += PushInst( n.v )
        prog += UnifyInst
    }

  def ground( term: TermAST ): Boolean =
    term match {
      case StructureAST( _, _, args ) => args forall ground
      case AtomAST( _, _ ) | _: NumericAST => true
      case AnonymousAST( _ ) | VariableAST( _, _ ) => false
    }

  def constant( term: TermAST ): Any =
    term match {
      case StructureAST( _, name, args ) => Structure( functor(name, args.length), args map constant toArray )
      case AtomAST( _, name ) => Symbol( name )
      case n: NumericAST => n.v
    }

  def compileTerm( term: TermAST )( implicit prog: Program, vars: Vars ): Unit =
    term match {
      case s: StructureAST if ground( s ) =>
        dbg( s"put structure", s.pos )
        prog += PushInst( constant(s) )
      case StructureAST( r, name, args ) =>
        dbg( s"put structure", r )
        args foreach compileTerm
        prog += StructureInst( functor(name, args.length) )
      case AtomAST( r, name ) =>
        dbg( "put atom", r )
        prog += PushInst( Symbol(name) )
      case AnonymousAST( r ) =>
        dbg( "put anonymous", r )
        prog += VarInst( vars.anon )
      case VariableAST( r, name ) =>
        dbg( "put variable", r )
        prog += VarInst( vars.num(name) )
      case n: NumericAST =>
        dbg( "put number", n.pos )
        prog += PushInst( n.v )
    }

  def compileArithmetic( expr: TermAST )( implicit prog: Program, vars: Vars ) {
    val exprvars = new mutable.HashMap[String, (Reader, Int, Int)]

    def addvar( term: TermAST )( implicit vars: Vars ): Unit =
      term match {
        case v@VariableAST( r, name ) =>
          vars get name match {
            case None => r.error( s"variable '$name' does not occur previously in the clause" )
            case Some( n ) =>
              v.name += '\''
              exprvars(name) = (r, n, vars.num( v.name ))
          }
        case StructureAST( _, _, args ) => args foreach addvar
        case _ =>
      }

    addvar( expr )

    for ((n, (r, v, v1)) <- exprvars if vars eval n)
      prog += EvalInst( r, n, v, v1 )
  }

  def compileExpression( expr: TermAST )( implicit prog: Program, vars: Vars ): Unit =
    expr match {
      case x: NumericAST => prog += PushInst( x.v )
      case VariableAST( pos, name ) => prog += VarInst( vars.num(name) )
      case StructureAST( pos, op@("+"|"-"|"*"|"/"), List(left, right) ) =>
        compileExpression( left )
        compileExpression( right )
        prog +=
          (op match {
            case "+" => AddInst
            case "-" => SubInst
            case "*" => MulInst
            case "/" => DivInst
          })
      case StructureAST( pos, op@"-", List(arg) ) =>
        compileExpression( arg )
        prog +=
          (op match {
            case "-" => NegInst
          })
      case StructureAST( _, name, args ) if Math exists functor( name, args.length ) =>
        args foreach compileExpression
        prog += NativeInst( Math.function(functor(name, args.length)) )
      case StructureAST( pos, name, args ) => pos.error( s"function $name/${args.length} not found" )
      case AtomAST( _, name ) if Math exists functor( name, 0 ) =>
        prog += NativeInst( Math.function(functor( name, 0)) )
      case AtomAST( pos, name ) => pos.error( s"constant '$name' not found" )
    }

  def compileBody( ast: TermAST )( implicit prog: Program, vars: Vars ): Unit =
    ast match {
      case StructureAST( r1, ";", List(StructureAST( r, "->", List(goal1, goal2) ), goal3) ) =>
        dbg( s"if-then-else", r )
        prog.patch( (ptr, len) => MarkInst(len - ptr) ) { // need to skip over the branch
          compileBody( goal1 )
          prog += UnmarkInst
          dbg( s"then part", r )
          compileBody( goal2 ) }
        prog.patch( (ptr, len) => BranchInst(len - ptr - 1) ) {
          dbg( s"else part", r1 )
          compileBody( goal3 ) }
      case StructureAST( r, "->", List(goal1, goal2) ) =>
        dbg( s"if-then", r )
        prog.patch( (ptr, len) => MarkInst(len - ptr + 1) ) { // need to skip over the unmark/branch
          compileBody( goal1 ) }
        prog += UnmarkInst
        prog += BranchInst( 1 )
        prog += FailInst
        dbg( s"then part", r )
        compileBody( goal2 )
      case StructureAST( r, "\\+", List(term@(AtomAST(_, _) | StructureAST( _, _, _ ))) ) =>
        dbg( s"not provable", r )
        prog.patch( (ptr, len) => MarkInst(len - ptr + 1) ) { // need to skip over the unmark/fail
          compileBody( term ) }
        prog += UnmarkInst
        prog += FailInst
//      case StructureAST( r, "call", List(term@(AtomAST(_, _) | StructureAST( _, _, _ ))) ) =>
//        dbg( s"call", r )
//        prog.patch( (ptr, len) => MarkInst(len - ptr + 1) ) { // need to skip over the unmark/fail
//          compileBody( term ) }
//        prog += UnmarkInst
//      case StructureAST( r, "once", List(term@(AtomAST(_, _) | StructureAST( _, _, _ ))) ) =>
//        dbg( s"not provable", r )
//        prog.patch( (ptr, len) => MarkInst(len - ptr + 1) ) { // need to skip over the unmark/fail
//          compileBody( term ) }
//        prog += UnmarkInst
      case StructureAST( _, ",", List(left, right) ) =>
        compileBody( left )
        compileBody( right )
      case StructureAST( r, ";", List(left, right) ) =>
        dbg( "disjunction", r )
        prog.patch( (ptr, len) => ChoiceInst(len - ptr) ) { // need to skip over the branch
          compileBody( left ) }
        prog.patch( (ptr, len) => BranchInst(len - ptr - 1) ) {
          compileBody( right ) }
      case AtomAST( _, "true" ) =>  // no code to emit for true/0
      case AtomAST( r, "false"|"fail" ) =>
        dbg( "fail", r )
        prog += FailInst
      case AtomAST( r, "!" ) =>
        dbg( "cut", r )
        prog += CutInst
      case AtomAST( r, "repeat" ) =>
        dbg( "repeat", r )
        prog += ChoiceInst( -1 )
      case StructureAST( pos, "=", List(VariableAST(_, lname), right) ) =>
        dbg( "unify", pos )
        compileTerm( right )
        prog += VarUnifyInst( vars.num(lname) )
      case StructureAST( pos, "=", List(left, VariableAST(_, rname)) ) =>
        dbg( "unify", pos )
        compileTerm( left )
        prog += VarUnifyInst( vars.num(rname) )
      case StructureAST( pos, "=", List(left, right) ) =>
        dbg( "unify", pos )
        compileTerm( left )
        compileTerm( right )
        prog += UnifyInst
      case StructureAST( pos, "\\=", List(left, right) ) =>
        dbg( "not unifiable", pos )
        prog.patch( (ptr, len) => MarkInst(len - ptr - 1) ) {
          compileTerm( left )
          compileTerm( right )
          prog += UnifyInst
          prog += UnmarkInst
          prog += FailInst }
      case StructureAST( pos, "is", List(VariableAST(_, rname), expr) ) =>
        compileArithmetic( expr )
        compileExpression( expr )
        prog += VarInst( vars.num(rname) )
        prog += UnifyInst
      case StructureAST( _, "is", List(head, _) ) => head.pos.error( s"variable was expected" )
      case StructureAST( pos, "=:=", List(left, right) ) =>
        compileArithmetic( ast )
        compileExpression( left )
        compileExpression( right )
        prog += EqInst
      case StructureAST( pos, "=\\=", List(left, right) ) =>
        compileArithmetic( ast )
        compileExpression( left )
        compileExpression( right )
        prog += NeInst
      case StructureAST( pos, "<", List(left, right) ) =>
        compileArithmetic( ast )
        compileExpression( right )
        compileExpression( left )
        prog += LtInst
      case StructureAST( pos, "=<", List(left, right) ) =>
        compileArithmetic( ast )
        compileExpression( right )
        compileExpression( left )
        prog += LeInst
      case StructureAST( pos, ">", List(left, right) ) =>
        compileArithmetic( ast )
        compileExpression( right )
        compileExpression( left )
        prog += GtInst
      case StructureAST( pos, ">=", List(left, right) ) =>
        compileArithmetic( ast )
        compileExpression( right )
        compileExpression( left )
        prog += GeInst
      case StructureAST( r, name, args ) if prog.defined( name, args.length ) =>
        val f = functor( name, args.length )

        dbg( s"procedure $f", r )
        prog += PushFrameInst
        args foreach compileTerm

        val p = prog.procedure( f )

        if (p.entry == -1)
          prog.fixup( f )
        else
          prog += CallInst( p.block, p.entry )
      case StructureAST( r, name, args ) if Builtin exists functor( name, args.length ) =>
        val f = functor( name, args.length )

        dbg( s"built-in $f", r )
        args foreach compileTerm
        prog += NativeInst( Builtin.predicate(f) )
      case StructureAST( pos, name, args ) =>
        val f = functor( name, args.length )

        dbg( s"procedure (indirect) $f", pos )
        prog += PushFrameInst
        args foreach compileTerm
        prog += CallIndirectInst( pos, f )
      case AtomAST( r, name ) if prog.defined( name, 0 ) =>
        val f = functor( name, 0 )

        dbg( s"built-in $f", r )
        prog += PushFrameInst

        val p = prog.procedure( f )

        if (p.entry == -1)
          prog.fixup( f )
        else
          prog += CallInst( p.block, p.entry )
      case AtomAST( r, name ) if Builtin exists functor( name, 0 ) =>
        val f = functor( name, 0 )

        dbg( s"built-in $f", r )
        prog += NativeInst( Builtin.predicate(f) )
      case AtomAST( r, name ) =>
        val f = functor( name, 0 )

        dbg( s"boal $f", r )
        prog += PushFrameInst
        prog += CallIndirectInst( r, functor(name, 0) )
    }

}