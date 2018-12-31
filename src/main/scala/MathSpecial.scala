package xyz.hyperreal.prolog

import scala.util.Random._


object MathSpecial {

  def rnd: Number = nextDouble

  def rnd( a: Number ): Number = nextInt( a.intValue )

  def time: Number = BigInt( System.currentTimeMillis )

}