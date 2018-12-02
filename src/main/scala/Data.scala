package xyz.hyperreal.prolog


abstract class Data {
  def isAtomic: Boolean
}

case class CompoundData( functor: Functor, args: Vector[Data] ) extends Data {
  val isAtomic = false
}

trait Atomic {
  val isAtomic = true
}

abstract class NumericData extends Data with Atomic {val n: Number}
case class IntegerData( n: Integer ) extends NumericData
case class FloatData( n: java.lang.Double ) extends NumericData

case class AtomData( s: Symbol ) extends Data with Atomic