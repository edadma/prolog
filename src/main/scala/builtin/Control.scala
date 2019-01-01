package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.prolog.VM


object Control {

  def halt( vm: VM ) = sys.exit

  def halt( vm: VM, status: Any ) = sys.exit( status.asInstanceOf[Int] )

}
