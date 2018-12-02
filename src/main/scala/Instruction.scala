package xyz.hyperreal.prolog


abstract class Instruction
case class VarInst( n: Int ) extends Instruction
case class PushInst( d: Any ) extends Instruction
case object ReturnInst extends Instruction
case class CompoundInst( f: Functor ) extends Instruction
