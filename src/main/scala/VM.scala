package xyz.hyperreal.prolog


class VM( prog: Program ) {

  def interp( goal: TermAST ): Unit =
    goal match {
      case CompoundAST( _, name, args ) if prog.exists( name, args.length ) =>
        args foreach interpTerm
        call( prog.procedure(name, args.length).entry )
      case CompoundAST( pos, name, args ) => pos.error( s"procedure $name/${args.length} not defined" )
      case AtomAST( _, name ) if prog.exists( name, 0 ) =>
        call( prog.procedure( name, 0).entry )
      case AtomAST( pos, name ) => pos.error( s"procedure $name/0 not defined" )
    }

  def interpTerm( term: TermAST ): Unit =
    term match {
      case CompoundAST( pos, name, args ) =>
        args foreach interpTerm
        pushCompound( Functor(Symbol(name), args.length) )
      case AtomAST( pos, name ) =>
        pushAtomic( AtomData(Symbol(name)) )
      case WildcardAST( pos ) => pos.error( "wildcard not allowed here" )
      case VariableAST( pos, name ) => PushVarInstruction( vars.num(name) )
      case IntegerAST( pos, v ) => PushAtomicInst( IntegerData(v) )
      case FloatAST( pos, v ) => PushAtomicInst( FloatData(v) )
    }

  def pushCompound( f: Functor ): Unit = {

  }

  def pushAtomic( d: Data ): Unit = {

  }

  def call( entry: Int ): Unit = {

  }

}