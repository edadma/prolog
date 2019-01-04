package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.prolog.{Compilation, Program, VM, Vars, array2list}

import scala.collection.mutable.ArrayBuffer


object AllSolutions {

  def findall( vm: VM, template: Any, goal: Any, instances: Any ) =
    if (TypeTesting.callable( vm, goal )) {
      implicit val prog = new Program
      implicit val vars = new Vars
      val block = prog.block( "findall" )

      Compilation.compileGoal( goal, vm.prog )

      val results = new ArrayBuffer[Any]

      if (vm.runblock( block )) {
        println( 123)
        results += 123

        while (vm.rerunblock( block )) {
          println( 456)
          results += 123
        }
      }

      vm.unify( array2list(results), instances )
    } else
      sys.error( s"findall: goal must be callable" )

}