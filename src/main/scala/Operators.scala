package xyz.hyperreal.prolog

import scala.collection.mutable


object Operators {

  val optable = new mutable.HashSet[Operator]
  val opmap = new mutable.HashMap[(Symbol, Int), Operator]

  add( 1200, 'xfx, ':- )
  add( 1200, 'xfx, '--> )
  add( 1200, 'fx, ':- )
  add( 1200, 'fx, '?- )
  add( 1050,	'xfy, '-> )
  add( 1000,	'xfy, Symbol(",") )
  add( 900, 'fy, Symbol("\\+") )
  add( 700, 'xfx, Symbol("\\=") )
  add( 700, 'xfx, '= )
  add( 700, 'xfx, '== )
  add( 700, 'xfx, Symbol("\\==") )
  add( 700, 'xfx, '@< )
  add( 700, 'xfx, '@=< )
  add( 700, 'xfx, '@> )
  add( 700, 'xfx, '@>= )
  add( 700, 'xfx, Symbol("=..") )
  add( 700, 'xfx, 'is )
  add( 700, 'xfx, '=:= )
  add( 700, 'xfx, '=\= )
  add( 700, 'xfx, '< )
  add( 700, 'xfx, '=< )
  add( 700, 'xfx, '> )
  add( 700, 'xfx, '>= )
  add( 500, 'yfx, '+ )
  add( 500, 'yfx, '- )
  add( 500, 'yfx, '/\ )
  add( 500, 'yfx, Symbol("\\/") )
  add( 400, 'yfx, '* )
  add( 400, 'yfx, '/ )
  add( 400, 'yfx, '// )
  add( 400, 'yfx, 'rem )
  add( 400, 'yfx, 'mod )
  add( 400, 'yfx, '<< )
  add( 400, 'yfx, '>> )
  add( 200, 'xfx, '** )
  add( 200, 'xfy, '^ )
  add( 200, 'fy, '- )
  add( 200, 'fy, Symbol("\\") )

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