package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.prolog.{Compilation, Program, VM, Vars}

import scala.collection.mutable.ListBuffer


object AllSolutions {

  def findall( vm: VM, template: Any, goal: Any, instances: Any ) =
    if (TypeTesting.callable( vm, goal )) {
      implicit val prog = new Program
      implicit val vars = new Vars
      val block = prog.block( "findall" )

      Compilation.compileGoal( goal, vm.prog )

      val resultlist = new ListBuffer[Any]

      vm.pushFrame
      vm.call( block, 0 )
      if (vm.runblock( block ))
        case Some( r ) =>
          def results( res: Map[String, Any] ): Unit = {
            if (trace || debug)
              out.println( s"==> $res" )

            resultset += res

            if (fail)
              run( block ) match {
                case Some( r1 ) => results( r1 )
                case None =>
              }
          }

          results( r )
        case None =>
      }

    } else
      sys.error( s"findall: goal must be callable" )

}