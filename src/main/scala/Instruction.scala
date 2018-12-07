package xyz.hyperreal.prolog


abstract class Instruction
case class PushAtomicInst( d: Data ) extends Instruction
case class PushVarInstruction( n: Int ) extends Instruction
case class PushCompoundInst(f: Functor ) extends Instruction
case object ReturnInst extends Instruction
case class VarMatchInst( n: Int ) extends Instruction
case class AtomMatchInst( a: Symbol ) extends Instruction
case class FunctorMatchInst( f: Functor ) extends Instruction
case class IntegerMatchInst( n: Int ) extends Instruction
case class FloatMatchInst( n: Double ) extends Instruction
case object DupInst extends Instruction
case class ElementInst( n: Int ) extends Instruction
case object EqInst extends Instruction
case class BranchIfInst( n: Int ) extends Instruction
case object FailInst extends Instruction
case class ChoiceInst( disp: Int ) extends Instruction
case class CallInstruction( entry: Int ) extends Instruction
case object DropInst extends Instruction
