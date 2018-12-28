package xyz.hyperreal.prolog


object BuiltinRuntime {

  def compile( vm: VM, term: Any ) =
    if (term.isInstanceOf[vm.Variable])
      sys.error( "compile: unbound variable" )
    else


}