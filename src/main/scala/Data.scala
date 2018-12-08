package xyz.hyperreal.prolog


abstract class Data {
  def isAtomic: Boolean
}

trait Compound {
  def element( n: Int ): Data

  val isAtomic = false
}

case class CompoundData( functor: Functor, args: Vector[Data] ) extends Data with Compound {
  def element( n: Int ) = args( n )
}

trait Atomic {
  val isAtomic = true
}

abstract class NumericData extends Data with Atomic {val n: Number}
case class IntegerData( n: Integer ) extends NumericData
case class FloatData( n: java.lang.Double ) extends NumericData

case class AtomData( s: Symbol ) extends Data with Atomic