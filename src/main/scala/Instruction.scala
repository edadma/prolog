package xyz.hyperreal.prolog


abstract class Instruction
case class VarInst( n: Int ) extends Instruction
case class PushInst( d: Data ) extends Instruction
case object ReturnInst extends Instruction
case class CompoundInst( f: Functor ) extends Instruction
case class AtomMatchInst( a: Symbol ) extends Instruction
case class FunctorMatchInst( f: Functor ) extends Instruction
case object DupInst extends Instruction
case class ElementInst( n: Int ) extends Instruction
case object EqInst extends Instruction
case class BranchIfInst( n: Int ) extends Instruction
case object FailInst extends Instruction
