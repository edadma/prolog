package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher.Reader
import xyz.hyperreal.recursive_descent_parser._


object PrologParser {

  val rule1200 = new RuleRef[TermAST]
  val rule900 = new RuleRef[TermAST]
  val integer = new TokenClassRule( classOf[IntegerToken], (r, s) => IntegerAST(r, s.toInt), "expected integer" )
  val string = new TokenClassRule( classOf[StringToken], (r, s) => StringAST(r, s), "expected string" )
  val cut = Action[(Reader, String), AtomAST]( Rule.symbol("!"), {case (pos, _) => AtomAST(pos, "!")} )
  val anyAtom =
    Alternates(
      List(
        new TokenClassRule( classOf[AtomToken], (r, s) => AtomAST(r, s), "expected atom" ),
        new TokenClassRule( classOf[SymbolToken], (r, s) => AtomAST(r, s), "expected atom" ),
        new TokenClassRule( classOf[QuotedAtomToken], (r, s) => AtomAST(r, s), "expected atom" )
      ) )
  val anyNonSymbolAtom =
    Alternates(
      List(
        new TokenClassRule( classOf[AtomToken], (r, s) => AtomAST(r, s), "expected atom" ),
        new TokenClassRule( classOf[QuotedAtomToken], (r, s) => AtomAST(r, s), "expected atom" )
      ) )

  val primary =
    Alternates[TermAST]( List(
      cut,
      integer,
      string,
      Rule.middle( Rule.symbol("("), rule1200, Rule.symbol(")") ),
      Sequence[AtomAST, List[TermAST], StructureAST](
        anyAtom,
        Rule.middle(
          Rule.symbol("("),
          Rule.oneOrMoreSeparated(rule900, Rule.symbol(",")),
          Rule.symbol(")")),
        (atom, args) => StructureAST(atom.pos, atom.name, args) ),
      anyNonSymbolAtom,
      SequenceLeft(
        Sequence[(Reader, String), (List[TermAST], Option[TermAST]), ListAST](
          Rule.symbol("["),
          Sequence[List[TermAST], Option[TermAST], (List[TermAST], Option[TermAST])](
            Rule.oneOrMoreSeparated(rule900, Rule.symbol(",")),
            Optional(SequenceRight(Rule.symbol("|"), rule1200)), (_, _) ), {case ((pos, _), (l, t)) => ListAST(pos, l, t)} ),
        Rule.symbol("]")),
      Sequence[(Reader, String), (Reader, String), AtomAST]( Rule.symbol("["), Rule.symbol("]"), (a, _) => AtomAST(a._1, "[]") )
    ) )
  val (rules, ops) = Builder[TermAST]( primary,
    List(
      Op(1200, 'xfx, ":-"),
      Op(1200, 'xfx, "-->"),
      Op(1200, 'fx, ":-"),
      Op(1200, 'fx, "?-"),
      Op(1100, 'xfy, ";"),
      Op(1050, 'xfy, "->"),
      Op(1000, 'xfy, ","),
      Op(900, 'fy, "\\+"),
      Op(700, 'xfx, "="),
      Op(700, 'xfx, "\\="),
      Op( 700, 'xfx, "==" ),
      Op( 700, 'xfx, "\\==" ),
      Op( 700, 'xfx, "@<" ),
      Op( 700, 'xfx, "@=<" ),
      Op( 700, 'xfx, "@>" ),
      Op( 700, 'xfx, "@>=" ),
      Op( 700, 'xfx, "=.." ),
      Op( 700, 'xfx, "is" ),
      Op( 700, 'xfx, "=:=" ),
      Op( 700, 'xfx, "=\\=" ),
      Op( 700, 'xfx, "<" ),
      Op( 700, 'xfx, "=<" ),
      Op( 700, 'xfx, ">" ),
      Op( 700, 'xfx, ">=" ),
      Op(500, 'yfx, "+"),
      Op(500, 'yfx, "-"),
      Op(500, 'yfx, "/\\"),
      Op(500, 'yfx, "\\/"),
      Op(400, 'yfx, "*"),
      Op(400, 'yfx, "/"),
      Op(400, 'yfx, "//"),
      Op(400, 'yfx, "rem"),
      Op(400, 'yfx, "mod"),
      Op(400, 'yfx, "<<"),
      Op(400, 'yfx, ">>"),
      Op(200, 'xfx, "**"),
      Op(200, 'xfy, "^"),
      Op(200, 'fy, "-"),
      Op(200, 'fy, "\\")), (r, s, x) => StructureAST( r, s, List(x) ), (x, r, s, y) => StructureAST( r, s, List(x, y) ) )

  rule1200.ref = rules(1200)
  rule900.ref = rules(900)

  val parser = new Parser( rules(1200), ops ++ List("(", ")", ".", "[", "]", "|", "!") )

  def expression = rules(1200)


}