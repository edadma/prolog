package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.Reader


abstract class Instruction
case class PushInst( a: Any ) extends Instruction
case class VarInst( n: Int ) extends Instruction
case class StructureInst( f: Functor ) extends Instruction
case class ElementInst( n: Int ) extends Instruction
case object ReturnInst extends Instruction
//case class BindInst( n: Int ) extends Instruction
case class FunctorInst( f: Functor ) extends Instruction
case object DupInst extends Instruction
case object EqInst extends Instruction
case class BranchIfInst( disp: Int ) extends Instruction
case object FailInst extends Instruction
case class ChoiceInst( disp: Int ) extends Instruction
case class CallInst( entry: Int ) extends Instruction
case class CallIndirectInst( pos: Reader, f: Functor ) extends Instruction
case object DropInst extends Instruction
case class FrameInst( vars: Int ) extends Instruction
case class PredicateInst( pred: VM => Unit ) extends Instruction
case object PushFrameInst extends Instruction
case object UnifyInst extends Instruction
