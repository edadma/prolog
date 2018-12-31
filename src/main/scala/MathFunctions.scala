package xyz.hyperreal.prolog

import xyz.hyperreal.lia


object MathFunctions {

  def abs( a: Number ) = lia.Math.absFunction( a )

  def sqrt( a: Number ) = lia.Math.sqrtFunction( a )

  def acos( a: Number ) = lia.Math.acosFunction( a )

  def asin( a: Number ) = lia.Math.asinFunction( a )

  def ceiling( a: Number ) = lia.Math.ceilFunction( a )

  def cos( a: Number ) = lia.Math.cosFunction( a )

  def sin( a: Number ) = lia.Math.sinFunction( a )

  def exp( a: Number ) = lia.Math.expFunction( a )

  def floor( a: Number ) = lia.Math.floorFunction( a )

  def float( a: Number ): Number =
    a match {
      case _: java.lang.Double | _: BigDecimal => a
      case _: Integer | _: BigInt => a.doubleValue
    }

}