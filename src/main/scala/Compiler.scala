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
          compileGoal( body, prog ) }
        prog += ReturnInst
      case StructureAST( r, ":-", List(AtomAST(pos, n), body) ) =>
        dbg( s"rule $n/0", pos )
        prog.patch( (_, _) => FrameInst(vars.count) ) {
          compileGoal( body, prog ) }
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

  def ground( term: Any ): Boolean =
    term match {
      case Structure( _, args ) => args forall ground
      case _: Symbol | _: Number => true
      case _: VM#Variable => false
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
    val seen = new mutable.HashMap[String, VariableAST]
    val exprvars = new mutable.HashMap[String, (Reader, Int, Int)]

    def addvar( term: TermAST )( implicit vars: Vars ): Unit =
      term match {
        case v@VariableAST( _, name ) if !seen.contains(name) =>
          seen(name) = v
          v.eval = true
        case v@VariableAST( r, name ) =>
          vars get name match {
            case None => r.error( s"variable '$name' does not occur previously in the clause" )
            case Some( n ) =>
              seen(name).name += '\''
              seen(name).eval = false
              v.name += '\''
              exprvars(name) = (r, n, vars.num( v.name ))
          }
        case StructureAST( _, _, args ) => args foreach addvar
        case _ =>
      }

    addvar( expr )

    for ((n, (r, v, v1)) <- exprvars if vars eval n) {
      prog += EvalInst( r, n, v )
      prog += VarUnifyInst( v1 )
    }
  }

  def compileExpression( expr: TermAST )( implicit prog: Program, vars: Vars ): Unit =
    expr match {
      case x: NumericAST => prog += PushInst( x.v )
      case v@VariableAST( pos, name ) if v.eval => prog += EvalInst( pos, name, vars.num(name) )
      case VariableAST( _, name ) => prog += VarInst( vars.num(name) )
      case StructureAST( pos, op@("+"|"-"|"*"|"/"|"mod"), List(left, right) ) =>
        compileExpression( left )
        compileExpression( right )
        prog +=
          (op match {
            case "+" => AddInst
            case "-" => SubInst
            case "*" => MulInst
            case "/" => DivInst
            case "mod" => ModInst
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

  def compileGoal( ast: TermAST, lookup: Program )( implicit prog: Program, vars: Vars ): Unit =
    ast match {
      case StructureAST( r1, ";", List(StructureAST( r, "->", List(goal1, goal2) ), goal3) ) =>
        dbg( s"if-then-else", r )
        prog.patch( (ptr, len) => MarkInst(len - ptr) ) { // need to skip over the branch
          compileGoal( goal1, lookup )
          prog += UnmarkInst
          dbg( s"then part", r )
          compileGoal( goal2, lookup ) }
        prog.patch( (ptr, len) => BranchInst(len - ptr - 1) ) {
          dbg( s"else part", r1 )
          compileGoal( goal3, lookup ) }
      case StructureAST( r, "->", List(goal1, goal2) ) =>
        dbg( s"if-then", r )
        prog.patch( (ptr, len) => MarkInst(len - ptr + 1) ) { // need to skip over the unmark/branch
          compileGoal( goal1, lookup ) }
        prog += UnmarkInst
        prog += BranchInst( 1 )
        prog += FailInst
        dbg( s"then part", r )
        compileGoal( goal2, lookup )
      case StructureAST( r, "\\+", List(term@(AtomAST(_, _) | StructureAST( _, _, _ ))) ) =>
        dbg( s"not provable", r )
        prog.patch( (ptr, len) => MarkInst(len - ptr + 1) ) { // need to skip over the unmark/fail
          compileGoal( term, lookup ) }
        prog += UnmarkInst
        prog += FailInst
      case StructureAST( r, "call", List(term@(AtomAST(_, _) | StructureAST( _, _, _ ))) ) =>
        dbg( s"call", r )
        prog.patch( (ptr, len) => MarkInst(len - ptr + 1) ) { // need to skip over the unmark/fail
          compileGoal( term, lookup ) }
        prog += UnmarkInst
      case StructureAST( r, "call", List(VariableAST(pos, name)) ) =>
        dbg( s"call (compile)", r )
        prog += PushFrameInst
        prog += VarInst( vars.num(name) )
        prog += NativeInst( Runtime.compileCall )
        prog += MarkInst( 2 )
        prog += CallBlockInst
        prog += UnmarkInst
      case StructureAST( r, "call", List(arg) ) => r.error( s"call: term should be callable: $arg" )
      case StructureAST( r, "once", List(term@(AtomAST(_, _) | StructureAST( _, _, _ ))) ) =>
        dbg( s"once", r )
        prog.patch( (ptr, len) => MarkInst(len - ptr) ) { // need to skip over the unmark
          compileGoal( term, lookup ) }
        prog += UnmarkInst
      case StructureAST( _, ",", List(left, right) ) =>
        compileGoal( left, lookup )
        compileGoal( right, lookup )
      case StructureAST( r, ";", List(left, right) ) =>
        dbg( "disjunction", r )
        prog.patch( (ptr, len) => ChoiceInst(len - ptr) ) { // need to skip over the branch
          compileGoal( left, lookup ) }
        prog.patch( (ptr, len) => BranchInst(len - ptr - 1) ) {
          compileGoal( right, lookup ) }
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
      case StructureAST( pos, "is", List(VariableAST(_, lname), expr) ) =>
        compileArithmetic( expr )
        compileExpression( expr )
        prog += VarUnifyInst( vars.num(lname) )
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
      case StructureAST( r, name, args ) if lookup.defined( name, args.length ) =>
        val f = functor( name, args.length )

        dbg( s"procedure $f", r )
        prog += PushFrameInst
        args foreach compileTerm

        val p = lookup.procedure( f )

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
      case AtomAST( r, name ) if lookup.defined( name, 0 ) =>
        val f = functor( name, 0 )

        dbg( s"built-in $f", r )
        prog += PushFrameInst

        val p = lookup.procedure( f )

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

        dbg( s"procedure (indirect) $f", r )
        prog += PushFrameInst
        prog += CallIndirectInst( r, functor(name, 0) )
    }

  def compileTerm( term: Any )( implicit prog: Program, vars: Vars ): Unit =
    term match {
      case s: Structure if ground( s ) => prog += PushInst( s )
      case Structure( f, args ) =>
        args foreach compileTerm
        prog += StructureInst( f )
      case s: Symbol => prog += PushInst( s )
//      case AnonymousAST( r ) =>
//        prog += VarInst( vars.anon )
//      case VariableAST( r, name ) =>
//        prog += VarInst( vars.num(name) )
      case n: NumericAST => prog += PushInst( n.v )
    }

  /*
  def compileArithmetic( expr: Any )( implicit prog: Program, vars: Vars ) {
    val seen = new mutable.HashMap[String, VariableAST]
    val exprvars = new mutable.HashMap[String, (Reader, Int, Int)]

    def addvar( term: TermAST )( implicit vars: Vars ): Unit =
      term match {
        case v@VariableAST( _, name ) if !seen.contains(name) =>
          seen(name) = v
          v.eval = true
        case v@VariableAST( r, name ) =>
          vars get name match {
            case None => r.error( s"variable '$name' does not occur previously in the clause" )
            case Some( n ) =>
              seen(name).name += '\''
              seen(name).eval = false
              v.name += '\''
              exprvars(name) = (r, n, vars.num( v.name ))
          }
        case StructureAST( _, _, args ) => args foreach addvar
        case _ =>
      }

    addvar( expr )

    for ((n, (r, v, v1)) <- exprvars if vars eval n) {
      prog += EvalInst( r, n, v )
      prog += VarUnifyInst( v1 )
    }
  }

  def compileExpression( expr: Any )( implicit prog: Program, vars: Vars ): Unit =
    expr match {
      case x: NumericAST => prog += PushInst( x.v )
      case v@VariableAST( pos, name ) if v.eval => prog += EvalInst( pos, name, vars.num(name) )
      case VariableAST( _, name ) => prog += VarInst( vars.num(name) )
      case StructureAST( pos, op@("+"|"-"|"*"|"/"|"mod"), List(left, right) ) =>
        compileExpression( left )
        compileExpression( right )
        prog +=
          (op match {
            case "+" => AddInst
            case "-" => SubInst
            case "*" => MulInst
            case "/" => DivInst
            case "mod" => ModInst
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
*/

  def compileGoal( data: Any, lookup: Program )( implicit prog: Program, vars: Vars ): Unit =
    data match {
      case Structure( Functor(Symbol(";"), 2), Array(Structure(Functor(Symbol("->"), 2), Array(goal1, goal2)), goal3) ) =>
        prog.patch( (ptr, len) => MarkInst(len - ptr) ) { // need to skip over the branch
          compileGoal( goal1, lookup )
          prog += UnmarkInst
          compileGoal( goal2, lookup ) }
        prog.patch( (ptr, len) => BranchInst(len - ptr - 1) ) {
          compileGoal( goal3, lookup ) }
      case Structure( Functor(Symbol("->"), 2), Array(goal1, goal2) ) =>
        prog.patch( (ptr, len) => MarkInst(len - ptr + 1) ) { // need to skip over the unmark/branch
          compileGoal( goal1, lookup ) }
        prog += UnmarkInst
        prog += BranchInst( 1 )
        prog += FailInst
        compileGoal( goal2, lookup )
      case Structure( Functor(Symbol("\\+"), 1), Array(term@(_: Symbol | _: Structure)) ) =>
        prog.patch( (ptr, len) => MarkInst(len - ptr + 1) ) { // need to skip over the unmark/fail
          compileGoal( term, lookup ) }
        prog += UnmarkInst
        prog += FailInst
      //      case StructureAST( r, "call", List(term@(AtomAST(_, _) | StructureAST( _, _, _ ))) ) =>
      //        dbg( s"call", r )
      //        prog.patch( (ptr, len) => MarkInst(len - ptr + 1) ) { // need to skip over the unmark/fail
      //          compileBody( term ) }
      //        prog += UnmarkInst
//      case StructureAST( r, "once", List(term@(AtomAST(_, _) | StructureAST( _, _, _ ))) ) =>
//        dbg( s"once", r )
//        prog.patch( (ptr, len) => MarkInst(len - ptr) ) { // need to skip over the unmark
//          compileGoal( term, lookup ) }
//        prog += UnmarkInst
      case Structure( Functor(Symbol(","), 2), Array(left, right) ) =>
        compileGoal( left, lookup )
        compileGoal( right, lookup )
      case Structure( Functor(Symbol(";"), 2), Array(left, right) ) =>
        prog.patch( (ptr, len) => ChoiceInst(len - ptr) ) { // need to skip over the branch
          compileGoal( left, lookup ) }
        prog.patch( (ptr, len) => BranchInst(len - ptr - 1) ) {
          compileGoal( right, lookup ) }
      case 'true =>  // no code to emit for true/0
      case 'false|'fail => prog += FailInst
      case Symbol( "!" ) => prog += CutInst
      case 'repeat => prog += ChoiceInst( -1 )
//      case Structure( Functor(Symbol("="), 2), Array(v: VM#Variable, right) ) =>
//        compileTerm( right )
//        prog += VarUnifyInst( vars.num(lname) )
//      case StructureAST( pos, "=", List(left, VariableAST(_, rname)) ) =>
//        compileTerm( left )
//        prog += VarUnifyInst( vars.num(rname) )
      case Structure( Functor(Symbol("="), 2), Array(left, right) ) =>
        compileTerm( left )
        compileTerm( right )
        prog += UnifyInst
      case Structure( Functor(Symbol("\\="), 2), Array(left, right) ) =>
        prog.patch( (ptr, len) => MarkInst(len - ptr - 1) ) {
          compileTerm( left )
          compileTerm( right )
          prog += UnifyInst
          prog += UnmarkInst
          prog += FailInst }
//      case StructureAST( pos, "is", List(VariableAST(_, rname), expr) ) =>
//        compileArithmetic( expr )
//        compileExpression( expr )
//        prog += VarInst( vars.num(rname) )
//        prog += UnifyInst
//      case StructureAST( _, "is", List(head, _) ) => head.pos.error( s"variable was expected" )
//      case StructureAST( pos, "=:=", List(left, right) ) =>
//        compileArithmetic( data )
//        compileExpression( left )
//        compileExpression( right )
//        prog += EqInst
//      case StructureAST( pos, "=\\=", List(left, right) ) =>
//        compileArithmetic( data )
//        compileExpression( left )
//        compileExpression( right )
//        prog += NeInst
//      case StructureAST( pos, "<", List(left, right) ) =>
//        compileArithmetic( data )
//        compileExpression( right )
//        compileExpression( left )
//        prog += LtInst
//      case StructureAST( pos, "=<", List(left, right) ) =>
//        compileArithmetic( data )
//        compileExpression( right )
//        compileExpression( left )
//        prog += LeInst
//      case StructureAST( pos, ">", List(left, right) ) =>
//        compileArithmetic( data )
//        compileExpression( right )
//        compileExpression( left )
//        prog += GtInst
//      case StructureAST( pos, ">=", List(left, right) ) =>
//        compileArithmetic( data )
//        compileExpression( right )
//        compileExpression( left )
//        prog += GeInst
      case Structure( f, args ) if lookup defined f =>
        prog += PushFrameInst
        args foreach compileTerm

        val p = lookup.procedure( f )

        if (p.entry == -1)
          sys.error( s"procedure entry point unknown: $p" )
        else
          prog += CallInst( p.block, p.entry )
      case Structure( f, args ) if Builtin exists f =>
        args foreach compileTerm
        prog += NativeInst( Builtin.predicate(f) )
      case Structure( f, args ) =>
        prog += PushFrameInst
        args foreach compileTerm
        prog += CallIndirectInst( null, f )
      case a: Symbol if lookup defined Functor( a, 0 ) =>
        val f = Functor( a, 0 )

        prog += PushFrameInst

        val p = lookup.procedure( f )

        if (p.entry == -1)
          sys.error( s"procedure entry point unknown: $p" )
        else
          prog += CallInst( p.block, p.entry )
      case a: Symbol if Builtin exists Functor( a, 0 ) => prog += NativeInst( Builtin predicate Functor( a, 0 ) )
      case a: Symbol =>
        prog += PushFrameInst
        prog += CallIndirectInst( null, Functor( a, 0 ) )
    }

}