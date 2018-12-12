package xyz.hyperreal.prolog


trait Compound {
  def element( n: Int ): Any

  val isAtomic = false
}

case class Structure( functor: Functor, args: Vector[Any] ) extends Compound {
  def element( n: Int ) = args( n )
}