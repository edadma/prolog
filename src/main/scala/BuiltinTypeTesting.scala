package xyz.hyperreal.prolog


object BuiltinTypeTesting {

  def `var`( vm: VM, a: AnyRef ) = a.isInstanceOf[VM#Variable]

  def novar( vm: VM, a: AnyRef ) = !a.isInstanceOf[VM#Variable]

  def atom( vm: VM, a: AnyRef ) = a.isInstanceOf[Symbol]

  def integer( vm: VM, a: AnyRef ) = {println(a.isInstanceOf[Int] || a.isInstanceOf[BigInt]);a.isInstanceOf[Int] || a.isInstanceOf[BigInt]}

  def float( vm: VM, a: AnyRef ) = a.isInstanceOf[Double] || a.isInstanceOf[BigDecimal]

  def number( vm: VM, a: AnyRef ) = a.isInstanceOf[Number]

  def compound( vm: VM, a: AnyRef ) = a.isInstanceOf[Structure]

  def atomic( vm: VM, a: AnyRef ) = novar( vm, a ) && !compound( vm, a )

  def callable( vm: VM, a: AnyRef ) = atom( vm, a ) || compound( vm, a )

  def ground( vm: VM, a: AnyRef ): Boolean =
    atomic( vm, a ) ||
      (vareval( a ) match {
        case Structure( _, args ) => args forall (ground( vm, _ ))
        case _ => false
      })

}