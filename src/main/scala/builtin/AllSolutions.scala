package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.prolog.{Compilation, Program, VM, Vars}


object AllSolutions {

  def findall( vm: VM, template: Any, goal: Any, instances: Any ) =
    if (TypeTesting.callable( vm, goal )) {
      implicit val prog = new Program
      implicit val vars = new Vars
      val block = prog.block( "findall" )

      Compilation.compileGoal( goal, vm.prog )

    } else
      sys.error( s"findall: goal must be callable" )

}