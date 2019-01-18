package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.Reader

import scala.collection.mutable


object Compilation {

  var debug = false

  val reserved =
    Set(
      indicator( "true", 0 ),
      indicator( "fail", 0 ),
      indicator( "false", 0 ),
      indicator( "repeat", 0 ),
      indicator( "is", 2 )
    )

  def compile( ast: PrologAST, prog: Program ): Unit = {
    phase1( ast, prog )
    phase2( prog )
  }

  def phase1( ast: PrologAST, prog: Program ): Unit =
    ast match {
      case SourceAST( clauses ) => clauses foreach (phase1( _, prog ))
      case ClauseAST( StructureAST(r, ":-", List(StructureAST(r1, "import", List(AtomAST(_, name))))) ) =>
        prog.loadResource( name )
      case ClauseAST( StructureAST(r, ":-", List(StructureAST(r1, "import", List(StringAST(_, name))))) ) =>
        prog.loadResource( name )
      case ClauseAST( clause@StructureAST(r, ":-", List(head@StructureAST(h, name, args), body)) ) =>
        val f = indicator( name, args.length )

        if (Builtin.exists( f ) || Math.exists( f ) || reserved(f))
          r.error( s"builtin procedure '$f' can't be redefined" )

        prog.clause( f, clause, head, body )
      case ClauseAST( clause@StructureAST(r, ":-", List(head@AtomAST(h, name), body)) ) =>
        val f = indicator( name, 0 )

        if (Builtin.exists( f ) || Math.exists( f ) || reserved(f))
          r.error( s"builtin procedure '$f' can't be redefined" )

        prog.clause( f, clause, head, body )
      case ClauseAST( clause@StructureAST(r, name, args) ) =>
        val f = indicator( name, args.length )

        if (Builtin.exists( f ) || Math.exists( f ) || reserved(f))
          r.error( s"builtin procedure '$f' can't be redefined" )

        prog.clause( f, clause, clause, TRUE )
      case ClauseAST( clause@AtomAST(r, name) ) =>
        val f = indicator( name, 0 )

        if (Builtin.exists( f ) || Math.exists( f ) || reserved(f))
          r.error( s"builtin procedure '$f' can't be redefined" )

        prog.clause( f, clause, clause, TRUE )
    }

  def phase2( implicit prog: Program ) =
    prog.procedures foreach {
      case proc@Procedure( f, block, pub, clauses ) if (block == null || block.length == 0) && (clauses.isEmpty || clauses.head.block.length == 0) =>
        if (!pub)
          proc.block = prog.block( f.toString )

        var jumpblock: Block = null
        var jumpidx = 0

        for ((c, i) <- clauses.init.zipWithIndex) {
          if (pub)
            prog.block( s"$f ${i + 1}" )

          if (jumpblock ne null)
            jumpblock(jumpidx) = JumpInst( c.block )

          prog.patch( (ptr, len) => CutChoiceInst(len - ptr - 1) ) {
            compileClause( c.ast )
          }

          jumpblock = c.block
          jumpidx = c.block.length

          prog += null
        }

        if (pub)
          prog.block( s"$f ${clauses.length}" )

        if (jumpblock ne null)
          jumpblock(jumpidx) = JumpInst( clauses.last.block )

        compileClause( clauses.last.ast )
      case _ =>
    }

  def dbg( msg: String, pos: Reader )( implicit prog: Program ) =
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

  def compileClause( term: Any )( implicit prog: Program ) = {
    implicit val vars = new Vars

    term match {
      case Structure( Indicator(Symbol(":-"), 2), Array(Structure(f, args), body) ) =>
        prog.patch( (_, _) => FrameInst(vars.count) ) {
          args.reverse foreach compileHead
          compileGoal( body, prog ) }
        prog += ReturnInst
      case StructureAST( r, ":-", List(AtomAST(pos, n), body) ) =>
        prog.patch( (_, _) => FrameInst(vars.count) ) {
          compileGoal( body, prog ) }
        prog += ReturnInst
      case StructureAST( r, f, args ) =>
        prog.patch( (_, _) => FrameInst(vars.count) ) {
          args.reverse foreach compileHead }
        prog += ReturnInst
      case AtomAST( r, name ) =>
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
      case AtomAST( r, "[]" ) =>
        dbg( "get nil", r )
        prog += NilUnifyInst
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
        prog += FunctorInst( Indicator(Symbol(name), args.length) )

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

  def compileHead( term: Any )( implicit prog: Program, vars: Vars ): Unit =
    term match {
      case AtomAST( r, "[]" ) =>
        dbg( "get nil", r )
        prog += NilUnifyInst
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
        prog += FunctorInst( Indicator(Symbol(name), args.length) )

        args.zipWithIndex foreach {
          case (e, i) =>
            dbg( s"get arg $i", e.pos )

            if (i < args.length - 1)
              prog += DupInst

            compileTerm( e )
            prog += ElementUnifyInst( i )
        }
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

  def toTerm( term: TermAST ): Any =
    term match {
      case StructureAST( _, name, args ) => Structure( indicator(name, args.length), args map toTerm toArray )
      case AtomAST( _, name ) => Symbol( name )
      case n: NumericAST => n.v
      case AnonymousAST( _ ) | VariableAST( _, _ ) => false
    }

  def compileTerm( term: TermAST )( implicit prog: Program, vars: Vars ): Unit =
    term match {
      case s: StructureAST if ground( s ) =>
        dbg( s"put structure", s.pos )
        prog += PushInst( toTerm(s) )
      case StructureAST( r, name, args ) =>
        dbg( s"put structure", r )
        args foreach compileTerm
        prog += StructureInst( indicator(name, args.length) )
      case AtomAST( r, name ) =>
        dbg( "put atom", r )
        prog += PushInst( Symbol(name) )
      case AnonymousAST( r ) =>
        dbg( "put anonymous", r )
        prog += PushVarInst( vars.anon )
      case VariableAST( r, name ) =>
        dbg( "put variable", r )
        prog += PushVarInst( vars.num(name) )
      case n: NumericAST =>
        dbg( "put number", n.pos )
        prog += PushInst( n.v )
      case StringAST( r, s ) =>
        dbg( "put string", r )
        prog += PushInst( s )
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
      case VariableAST( _, name ) => prog += PushVarInst( vars.num(name) )
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
      case StructureAST( _, name, args ) if Math exists indicator( name, args.length ) =>
        val f = indicator(name, args.length)

        args foreach compileExpression
        prog += NativeInst( Math.function(f), Vector(), f, NATIVE_MATH )
      case StructureAST( pos, name, args ) => pos.error( s"function $name/${args.length} not found" )
      case AtomAST( _, name ) if Math exists indicator( name, 0 ) =>
        val f = indicator(name, 0)

        prog += NativeInst( Math.function(f), Vector(), f, NATIVE_MATH )
      case AtomAST( pos, name ) => pos.error( s"constant or system value '$name' not found" )
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
      case StructureAST( r, "var", List(term) ) =>
        dbg( "var", r )
        compileTerm( term )
        prog += VarInst
      case StructureAST( r, "nonvar", List(term) ) =>
        dbg( "nonvar", r )
        compileTerm( term )
        prog += NonvarInst
      case StructureAST( r, "call", List(term@(AtomAST(_, _) | StructureAST( _, _, _ ))) ) =>
        dbg( s"call", r )
        prog.patch( (ptr, len) => MarkInst(len - ptr + 1) ) { // need to skip over the unmark/fail
          compileGoal( term, lookup ) }
        prog += UnmarkInst
      case StructureAST( r, "call", List(VariableAST(pos, name)) ) =>
        dbg( s"call (compile)", r )
        prog += PushFrameInst
        prog += PushVarInst( vars.num(name) )
        prog += NativeInst( Runtime.compileCall, Vector(), Indicator('$compileCall, 0), NATIVE_RUNTIME )
        prog += MarkInst( 2 )
        prog += CallBlockInst
        prog += UnmarkInst
      case StructureAST( r, "call", List(arg) ) => r.error( s"call: term should be callable: $arg" )
      case StructureAST( r, "once", List(term@(AtomAST(_, _) | StructureAST( _, _, _ ))) ) =>
        dbg( s"once", r )
        prog.patch( (ptr, len) => MarkInst(len - ptr) ) { // need to skip over the unmark
          compileGoal( term, lookup ) }
        prog += UnmarkInst
      case StructureAST( r, "once", List(VariableAST(pos, name)) ) =>
        dbg( s"once (compile)", r )
        prog += PushFrameInst
        prog += PushVarInst( vars.num(name) )
        prog += NativeInst( Runtime.compileCall, Vector(), Indicator('$compileCall, 0), NATIVE_RUNTIME )
        prog += MarkInst( 2 )
        prog += CallBlockInst
        prog += UnmarkInst
      case StructureAST( r, "once", List(arg) ) => r.error( s"once: term should be callable: $arg" )
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
      case StructureAST( pos, "==", List(left, right) ) =>
        dbg( "term equals", pos )
        compileTerm( left )
        compileTerm( right )
        prog += TermEqInst
      case StructureAST( pos, "\\==", List(left, right) ) =>
        dbg( "term equals", pos )
        prog.patch( (ptr, len) => MarkInst(len - ptr + 1) ) { // need to skip over the unmark/fail
          compileTerm( left )
          compileTerm( right )
          prog += TermEqInst }
        prog += UnmarkInst
        prog += FailInst
      case StructureAST( pos, "@<", List(left, right) ) =>
        dbg( "term less than", pos )
        compileTerm( left )
        compileTerm( right )
        prog += TermLtInst
      case StructureAST( pos, "@>", List(left, right) ) =>
        dbg( "term equals", pos )
        prog.patch( (ptr, len) => MarkInst(len - ptr + 1) ) { // need to skip over the unmark/fail
          compileTerm( left )
          compileTerm( right )
          prog += TermLeInst }
        prog += UnmarkInst
        prog += FailInst
      case StructureAST( pos, "@=<", List(left, right) ) =>
        dbg( "term less than or equal", pos )
        compileTerm( left )
        compileTerm( right )
        prog += TermLeInst
      case StructureAST( pos, "@>=", List(left, right) ) =>
        dbg( "term greater than or equal", pos )
        prog.patch( (ptr, len) => MarkInst(len - ptr + 1) ) { // need to skip over the unmark/fail
          compileTerm( left )
          compileTerm( right )
          prog += TermLtInst }
        prog += UnmarkInst
        prog += FailInst
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
        val f = indicator( name, args.length )

        dbg( s"procedure $f", r )
        prog += PushFrameInst
        args foreach compileTerm
        prog += CallProcedureInst( lookup procedure f )
      case StructureAST( r, name, args ) if Builtin exists indicator( name, args.length ) =>
        val f = indicator( name, args.length )

        dbg( s"built-in $f", r )
        args foreach compileTerm
        prog += NativeInst( Builtin.predicate(f), args map (_.pos) toVector, f, NATIVE_PREDICATE )
      case StructureAST( pos, name, args ) =>
        val f = indicator( name, args.length )

        dbg( s"call procedure (indirect) $f", pos )
        prog += PushFrameInst
        args foreach compileTerm
        prog += CallIndirectInst( pos, f )
      case AtomAST( r, name ) if lookup.defined( name, 0 ) =>
        val f = indicator( name, 0 )

        dbg( s"built-in $f", r )
        prog += PushFrameInst
        prog += CallProcedureInst( lookup procedure f )
      case AtomAST( r, name ) if Builtin exists indicator( name, 0 ) =>
        val f = indicator( name, 0 )

        dbg( s"built-in $f", r )
        prog += NativeInst( Builtin.predicate(f), Vector(), f, NATIVE_PREDICATE )
      case AtomAST( r, name ) =>
        val f = indicator( name, 0 )

        dbg( s"procedure (indirect) $f", r )
        prog += PushFrameInst
        prog += CallIndirectInst( r, indicator(name, 0) )
      case _ => sys.error( s"illegal goal term: $ast" )
    }

  def compileTerm( term: Any )( implicit prog: Program, vars: Vars ): Unit =
    term match {
      case s: Structure if groundTerm( s ) => prog += PushInst( s )
      case Structure( f, args ) =>
        args foreach compileTerm
        prog += StructureInst( f )
//      case v: VM#Variable if v.name == "_" => prog += VarInst( vars.anon )
      case v: VM#Variable => prog += PushInst( v )//VarInst( vars.num(v.name) )
      case _ => prog += PushInst( term )
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
      case Structure( Indicator(Symbol(";"), 2), Array(Structure(Indicator(Symbol("->"), 2), Array(goal1, goal2)), goal3) ) =>
        prog.patch( (ptr, len) => MarkInst(len - ptr) ) { // need to skip over the branch
          compileGoal( goal1, lookup )
          prog += UnmarkInst
          compileGoal( goal2, lookup ) }
        prog.patch( (ptr, len) => BranchInst(len - ptr - 1) ) {
          compileGoal( goal3, lookup ) }
      case Structure( Indicator(Symbol("->"), 2), Array(goal1, goal2) ) =>
        prog.patch( (ptr, len) => MarkInst(len - ptr + 1) ) { // need to skip over the unmark/branch
          compileGoal( goal1, lookup ) }
        prog += UnmarkInst
        prog += BranchInst( 1 )
        prog += FailInst
        compileGoal( goal2, lookup )
      case Structure( Indicator(Symbol("\\+"), 1), Array(term@(_: Symbol | _: Structure)) ) =>
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
      case Structure( Indicator(Symbol(","), 2), Array(left, right) ) =>
        compileGoal( left, lookup )
        compileGoal( right, lookup )
      case Structure( Indicator(Symbol(";"), 2), Array(left, right) ) =>
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
      case Structure( Indicator(Symbol("="), 2), Array(left, right) ) =>
        compileTerm( left )
        compileTerm( right )
        prog += UnifyInst
      case Structure( Indicator(Symbol("\\="), 2), Array(left, right) ) =>
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
        prog += CallProcedureInst( lookup.procedure(f) )
      case Structure( f, args ) if Builtin exists f =>
        args foreach compileTerm
        prog += NativeInst( Builtin.predicate(f), Vector.fill(args.length)(null), f, NATIVE_PREDICATE )
      case Structure( f, args ) =>
        prog += PushFrameInst
        args foreach compileTerm
        prog += CallIndirectInst( null, f )
      case a: Symbol if lookup defined Indicator( a, 0 ) =>
        prog += PushFrameInst
        prog += CallProcedureInst( lookup.procedure(Indicator(a, 0)) )
      case a: Symbol if Builtin exists Indicator( a, 0 ) =>
        val f = Indicator( a, 0 )

        prog += NativeInst( Builtin predicate f, Vector(), f, NATIVE_PREDICATE )
      case a: Symbol =>
        prog += PushFrameInst
        prog += CallIndirectInst( null, Indicator( a, 0 ) )
    }

}