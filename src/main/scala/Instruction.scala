package xyz.hyperreal.prolog


abstract class Instruction
case class PushAtomicInst( d: Data ) extends Instruction
case class PushVarInst( n: Int ) extends Instruction
case class PushCompoundInst( f: Functor ) extends Instruction
case class PushElementInst( n: Int ) extends Instruction
case object ReturnInst extends Instruction
case class VarBindInst( n: Int ) extends Instruction
case class FunctorInst( f: Functor ) extends Instruction
case object DupInst extends Instruction
case object EqInst extends Instruction
case class BranchIfInst( disp: Int ) extends Instruction
case object FailInst extends Instruction
case class ChoiceInst( disp: Int ) extends Instruction
case class CallInst( entry: Int ) extends Instruction
case object DropInst extends Instruction
