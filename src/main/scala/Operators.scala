package xyz.hyperreal.prolog

import scala.collection.mutable


class Operators {

  val optable =
    mutable.HashSet[Operator] (
      Operator( 1200, 'xfx, ':- ),
      Operator( 1200, 'xfx, '--> ),
      Operator( 1200, 'fx, ':- ),
      Operator( 1200, 'fx, '?- ),
      Operator( 1050,	'xfy, '-> ),
      Operator( 1000,	'xfy, Symbol(",") ),
      Operator( 900, 'fy, Symbol("\\+") ),
      Operator( 700, 'xfx, Symbol("\\=") ),
      Operator( 700, 'xfx, '= ),
      Operator( 700, 'xfx, '== ),
      Operator( 700, 'xfx, Symbol("\\==") ),
      Operator( 700, 'xfx, '@< ),
      Operator( 700, 'xfx, '@=< ),
      Operator( 700, 'xfx, '@> ),
      Operator( 700, 'xfx, '@>= ),
      Operator( 700, 'xfx, Symbol("=..") ),
      Operator( 700, 'xfx, 'is ),
      Operator( 700, 'xfx, '=:= ),
      Operator( 700, 'xfx, '=\= ),
      Operator( 700, 'xfx, '< ),
      Operator( 700, 'xfx, '=< ),
      Operator( 700, 'xfx, '> ),
      Operator( 700, 'xfx, '>= ),
      Operator( 500, 'yfx, '+ ),
      Operator( 500, 'yfx, '- ),
      Operator( 500, 'yfx, '/\ ),
      Operator( 500, 'yfx, Symbol("\\/") ),
      Operator( 400, 'yfx, '* ),
      Operator( 400, 'yfx, '/ ),
      Operator( 400, 'yfx, '// ),
      Operator( 400, 'yfx, 'rem ),
      Operator( 400, 'yfx, 'mod ),
      Operator( 400, 'yfx, '<< ),
      Operator( 400, 'yfx, '>> ),
      Operator( 200, 'xfx, '** ),
      Operator( 200, 'xfy, '^ ),
      Operator( 200, 'fy, '- ),
      Operator( 200, 'fy, Symbol("\\") )
    )

}