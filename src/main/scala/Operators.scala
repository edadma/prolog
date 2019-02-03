package xyz.hyperreal.prolog

import scala.collection.mutable


object Operators {

  lazy val (optable, opmap) = {
    val _optable = new mutable.HashSet[Operator]
    val _opmap = new mutable.HashMap[(Symbol, Int), Operator]

    def init( priority: Int, specifier: Symbol, operator: Symbol ): Unit = {
      val op = Operator( priority, specifier, operator )

      _optable += op
      _opmap((operator, specifier.name.length - 1)) = op
    }

    init( 1200, 'xfx, ':- )
    init( 1200, 'xfx, '--> )
    init( 1200, 'fx, ':- )
    init( 1200, 'fx, '?- )
    init( 1050,	'xfy, '-> )
    init( 1000,	'xfy, Symbol(",") )
    init( 900, 'fy, Symbol("\\+") )
    init( 700, 'xfx, Symbol("\\=") )
    init( 700, 'xfx, '= )
    init( 700, 'xfx, '== )
    init( 700, 'xfx, Symbol("\\==") )
    init( 700, 'xfx, '@< )
    init( 700, 'xfx, '@=< )
    init( 700, 'xfx, '@> )
    init( 700, 'xfx, '@>= )
    init( 700, 'xfx, Symbol("=..") )
    init( 700, 'xfx, 'is )
    init( 700, 'xfx, '=:= )
    init( 700, 'xfx, '=\= )
    init( 700, 'xfx, '< )
    init( 700, 'xfx, '=< )
    init( 700, 'xfx, '> )
    init( 700, 'xfx, '>= )
    init( 500, 'yfx, '+ )
    init( 500, 'yfx, '- )
    init( 500, 'yfx, '/\ )
    init( 500, 'yfx, Symbol("\\/") )
    init( 400, 'yfx, '* )
    init( 400, 'yfx, '/ )
    init( 400, 'yfx, '// )
    init( 400, 'yfx, 'rem )
    init( 400, 'yfx, 'mod )
    init( 400, 'yfx, '<< )
    init( 400, 'yfx, '>> )
    init( 200, 'xfx, '** )
    init( 200, 'xfy, '^ )
    init( 200, 'fy, '- )
    init( 200, 'fy, Symbol("\\") )

    (_optable, _opmap)
  }

  def all = optable.iterator

  def priority( p: Int ) = all filter (_.priority == p)

  def specifier( s: Symbol ) = all filter (_.specifier == s)

  def operator( o: Symbol ) = all filter (_.operator == o)

  def defined( op: Symbol, arity: Int ) = opmap contains (op, arity)

  def defined( op: Symbol, specifier: Symbol ): Boolean = defined( op, specifier.name.length - 1 )

  def apply( op: Symbol, arity: Int ) = opmap(op, arity)

  def get( op: Symbol, arity: Int ) = opmap get (op, arity)

  def get( op: Symbol, specifier: Symbol ): Option[Operator] = get( op, specifier.name.length - 1 )

  def add( priority: Int, specifier: Symbol, operator: Symbol ): Unit = {
    if (defined( operator, specifier ))
      sys.error( "operator already defined" )

    val op = Operator( priority, specifier, operator )

    optable += op
    opmap((operator, specifier.name.length - 1)) = op
  }

}