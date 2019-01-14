package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.VM


object Control {

  def halt( vm: VM, pos: IndexedSeq[Reader] ) = sys.exit

  def halt( vm: VM, pos: IndexedSeq[Reader], status: Any ) = sys.exit( status.asInstanceOf[Int] )

}
