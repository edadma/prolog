package xyz.hyperreal.prolog


object BuiltinTermIO {

  def write( a: Any ) = {
    print( display(a) )
    true
  }

  private def display( a: Any ): String =
    a match {
      case Symbol( s ) => s
      case Structure( Functor(Symbol(name), _), args ) => s"$name(${args.map(display).mkString(",")})"
      case _ => a.toString
    }

}