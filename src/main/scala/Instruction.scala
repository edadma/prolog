package xyz.hyperreal.prolog


abstract class Instruction
case class VarInst( n: Int ) extends Instruction
case class ConstInstruction( c: Data ) extends Instruction
