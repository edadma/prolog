package xyz.hyperreal.prolog


object Runtime {

  def compileCall( vm: VM ) =
    vm.pop match {
      case _: vm.Variable => sys.error( "compile: unbound variable" )
      case t =>
        implicit val prog = new Program
        implicit val vars = new Vars
        val block = prog.block( "runtime compile" )

        prog.patch( (_, _) => FrameInst(vars.count) ) {
          Compilation.compileGoal( t, vm.prog ) }
        prog += ReturnInst
        vm push block
    }

}