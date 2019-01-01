package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.prolog.{Structure, VM, vareval}


object TypeTesting {

  def `var`( vm: VM, a: Any ) = a.isInstanceOf[VM#Variable]

  def novar( vm: VM, a: Any ) = !a.isInstanceOf[VM#Variable]

  def atom( vm: VM, a: Any ) = a.isInstanceOf[Symbol]

  def integer( vm: VM, a: Any ) = {println(a.isInstanceOf[Int] || a.isInstanceOf[BigInt]);a.isInstanceOf[Int] || a.isInstanceOf[BigInt]}

  def float( vm: VM, a: Any ) = a.isInstanceOf[Double] || a.isInstanceOf[BigDecimal]

  def number( vm: VM, a: Any ) = a.isInstanceOf[Number]

  def compound( vm: VM, a: Any ) = a.isInstanceOf[Structure]

  def atomic( vm: VM, a: Any ) = novar( vm, a ) && !compound( vm, a )

  def callable( vm: VM, a: Any ) = atom( vm, a ) || compound( vm, a )

  def ground( vm: VM, a: Any ): Boolean =
    atomic( vm, a ) ||
      (vareval( a ) match {
        case Structure( _, args ) => args forall (ground( vm, _ ))
        case _ => false
      })

}
