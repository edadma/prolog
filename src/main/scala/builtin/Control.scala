package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{PrologException, VM}


object Control {

  def halt( vm: VM, pos: IndexedSeq[Reader] ) = sys.exit

  def halt( vm: VM, pos: IndexedSeq[Reader], status: Any ) = sys.exit( status.asInstanceOf[Int] )

  def `throw`( vm: VM, pos: IndexedSeq[Reader], error: Any ) =
    error match {
      case _: vm.Variable => sys.error( "error must be given" )
      case e => throw new PrologException( "exception", e )
    }

}
