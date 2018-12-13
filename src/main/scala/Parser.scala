package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher._
import xyz.hyperreal.lia


object Parser extends Matchers[StringReader] {

  reserved += ("is", "rem", "mod")
  delimiters += (
    ":-", "-->",
    "?-",
    ";",
    "->",
    ",",
    "\\+",
    "=", "\\=",
    "==", "\\==", "@<", "@=<", "@>", "@>=",
    "=..",
    "=",
    "=:=", "=\\=", "<", "=<", ">", ">=",
    "+", "-", "/\\", "\\/",
    "*", "/", "//", "<<", ">>",
    "**",
    "^",
    "\\",
    ".", "[", "|", "]", "(", ")"
  )

  def unary( pos: Reader, o: String, x: TermAST ) =
    (o, x) match {
      case ("-", IntegerAST( r, v )) => IntegerAST( r, -v )
      case ("-", FloatAST( r, v )) => FloatAST( r, -v )
      case _ => StructureAST( pos, o, List(x) )
    }

  def binary( pos: Reader, o: String, x: TermAST, y: TermAST ) = StructureAST( pos, o, List(x, y) )

  def mkterm( result: Any ) =
    result match {
      case (r: Reader) ~ (o: String) ~ (x: TermAST) => unary( r, o, x )
      case (x: TermAST) ~ (r: Reader) ~ (o: String) => unary( r, o, x )
      case (x: TermAST) ~ (r: Reader) ~ (o: String) ~ (y: TermAST) => binary( r, o, x, y )
    }

  def source = matchall(rep1(clause)) ^^ SourceAST

  def clause = term <~ "." ^^ ClauseAST

  def query = matchall(term)

  def term = p1200

  def p1200 =
    p1100 ~ pos ~ (":-"|"-->") ~ p1100 ^^ mkterm |
    pos ~ (":-"|"?-") ~ p1100 ^^ mkterm |
    p1100

  def assoc( result: Any ) = {
    result match {
      case (first: TermAST) ~ rest =>
        (first /: rest.asInstanceOf[List[~[~[Reader, String], TermAST]]]) { case (x, p ~ f ~ y) => binary( p, f, x, y ) } }
  }

  def p1100: Matcher[TermAST] =
    p1050 ~ ";" ~ p1100 ^^ mkterm |
    p1050

  def p1050: Matcher[TermAST] =
    p1000 ~ pos ~ "->" ~ p1050 ^^ mkterm |
    p1000

  def p1000: Matcher[TermAST] =
    p900 ~ pos ~ "," ~ p1000 ^^ mkterm |
    p900

  def p900: Matcher[TermAST] =
    pos ~ "\\+" ~ p900 ^^ mkterm |
    p700

  def p700 =
    p500 ~ pos ~ ("="|"\\="|"=="|"\\=="|"@<"|"@=<"|"@>"|"@>="|"=.."|"is"|"=:="|"=\\="|"<"|"=<"|">"|">=") ~ p500 ^^ mkterm |
    p500

  def p500 = p400 ~ rep(pos ~ ("+"|"-"|"/\\"|"\\/") ~ p400) ^^ assoc

  def p400 = p200 ~ rep(pos ~ ("*"|"/"|"//"|"<<"|">>") ~ p200) ^^ assoc

  def p200: Matcher[TermAST] =
    p0 ~ pos ~ "**" ~ p0 ^^ mkterm |
    p0 ~ pos ~ "^" ~ p200 ^^ mkterm |
    pos ~ ("-"|"\\") ~ p200 ^^ mkterm |
    p0

  def p0 =
    pos ~ integerLit ^^ { case p ~ n => IntegerAST( p, n ) } |
    "(" ~> term <~ ")" |
    pos ~ identOrReserved ~ "(" ~ rep1sep(p900, ",") ~ ")" ^^ {
      case p ~ n ~ _ ~ a ~ _ => StructureAST( p, n, a ) } |
    pos ~ identOrReserved ^^ {
      case p ~ "_" => WildcardAST( p )
      case p ~ a if a.head.isLower => AtomAST( p, a )
      case p ~ a => VariableAST( p, a ) }

}