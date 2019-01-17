package xyz.hyperreal.prolog.builtin

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.prolog.{DataStream, Structure, VM, vareval}


object TypeTesting {

  def atom( vm: VM, pos: IndexedSeq[Reader], a: Any ) = a.isInstanceOf[Symbol]

  def integer( vm: VM, pos: IndexedSeq[Reader], a: Any ) = a.isInstanceOf[Int] || a.isInstanceOf[BigInt]

  def float( vm: VM, pos: IndexedSeq[Reader], a: Any ) = a.isInstanceOf[Double] || a.isInstanceOf[BigDecimal]

  def number( vm: VM, pos: IndexedSeq[Reader], a: Any ) = a.isInstanceOf[Number]

  def compound( vm: VM, pos: IndexedSeq[Reader], a: Any ) = a.isInstanceOf[Structure]

  def atomic( vm: VM, pos: IndexedSeq[Reader], a: Any ) = !a.isInstanceOf[VM#Variable] && !compound( vm, pos, a )

  def callable( vm: VM, pos: IndexedSeq[Reader], a: Any ) = atom( vm, pos, a ) || compound( vm, pos, a )

  def ground( vm: VM, pos: IndexedSeq[Reader], a: Any ): Boolean =
    atomic( vm, pos, a ) ||
      (vareval( a ) match {
        case Structure( _, args ) => args forall (ground( vm, pos, _ ))
        case _ => false
      })

  def string( vm: VM, pos: IndexedSeq[Reader], a: Any ) = a.isInstanceOf[String]

  def is_stream( vm: VM, pos: IndexedSeq[Reader], a: Any ) = a.isInstanceOf[DataStream]

}
