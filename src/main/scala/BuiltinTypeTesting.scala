package xyz.hyperreal.prolog


object BuiltinTypeTesting {

  def `var`( a: Any ) = a.isInstanceOf[VM#Variable]

  def novar( a: Any ) = !a.isInstanceOf[VM#Variable]

  def atom( a: Any ) = a.isInstanceOf[Symbol]

  def integer( a: Any ) = a.isInstanceOf[Int] || a.isInstanceOf[BigInt]

  def float( a: Any ) = a.isInstanceOf[Double] || a.isInstanceOf[BigDecimal]

  def number( a: Any ) = a.isInstanceOf[Number]

  def compound( a: Any ) = a.isInstanceOf[Structure]

  def atomic( a: Any ) = novar( a ) && !compound( a )

  def callable( a: Any ) = atom( a ) || compound( a )

  def ground( a: Any ): Boolean =
    atomic( a ) ||
      (vareval( a ) match {
        case Structure( _, args ) => args forall ground
        case _ => false
      })

}