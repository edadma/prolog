package xyz.hyperreal.prolog

import xyz.hyperreal.pattern_matcher._


//object OldParser extends Matchers[Reader] {
//
//  reserved += ("is", "rem", "mod")
//  delimiters += (
//    ":-", "-->",
//    "?-",
//    ";",
//    "->",
//    ",",
//    "\\+",
//    "=", "\\=",
//    "==", "\\==", "@<", "@=<", "@>", "@>=",
//    "=..",
//    "=",
//    "=:=", "=\\=", "<", "=<", ">", ">=",
//    "+", "-", "/\\", "\\/",
//    "*", "/", "//", "<<", ">>",
//    "**",
//    "^",
//    "\\",
//    ".", "[", "|", "]", "(", ")", "!"
//  )
//
//  override val lineComment = '%'
//
//  def unary( pos: Reader, o: String, x: TermAST ) =
//    (o, x) match {
//      case ("-", IntegerAST( r, v )) => IntegerAST( r, -v )
//      case ("-", FloatAST( r, v )) => FloatAST( r, -v )
//      case _ => StructureAST( pos, o, List(x) )
//    }
//
//  def binary( pos: Reader, o: String, x: TermAST, y: TermAST ) = StructureAST( pos, o, List(x, y) )
//
//  def mkterm( result: Any ) =
//    result match {
//      case (r: Reader) ~ (o: String) ~ (x: TermAST) => unary( r, o, x )
//      case (x: TermAST) ~ (r: Reader) ~ (o: String) => unary( r, o, x )
//      case (x: TermAST) ~ (r: Reader) ~ (o: String) ~ (y: TermAST) => binary( r, o, x, y )
//    }
//
//  def source = matchall(rep1(clause)) ^^ SourceAST
//
//  def clause = term <~ "." ^^ ClauseAST
//
//  def query = matchall(term)
//
//  def term = p1200
//
//  def p1200 =
//    p1100 ~ pos ~ (":-"|"-->") ~ p1100 ^^ mkterm |
//    pos ~ (":-"|"?-") ~ p1100 ^^ mkterm |
//    p1100
//
//  def assoc( result: Any ) = {
//    result match {
//      case (first: TermAST) ~ rest =>
//        (first /: rest.asInstanceOf[List[~[~[Reader, String], TermAST]]]) { case (x, p ~ f ~ y) => binary( p, f, x, y ) } }
//  }
//
//  def p1100: Matcher[TermAST] =
//    p1050 ~ pos ~ ";" ~ p1100 ^^ mkterm |
//    p1050
//
//  def p1050: Matcher[TermAST] =
//    p1000 ~ pos ~ "->" ~ p1050 ^^ mkterm |
//    p1000
//
//  def p1000: Matcher[TermAST] =
//    p900 ~ pos ~ "," ~ p1000 ^^ mkterm |
//    p900
//
//  def p900: Matcher[TermAST] =
//    pos ~ "\\+" ~ p900 ^^ mkterm |
//    p700
//
//  def p700 =
//    p500 ~ pos ~ ("="|"\\="|"=="|"\\=="|"@<"|"@=<"|"@>"|"@>="|"=.."|"is"|"=:="|"=\\="|"<"|"=<"|">"|">=") ~ p500 ^^ mkterm |
//    p500
//
//  def p500 = p400 ~ rep(pos ~ ("+"|"-"|"/\\"|"\\/") ~ p400) ^^ assoc
//
//  def p400 = p200 ~ rep(pos ~ ("*"|"/"|"//"|"<<"|">>"|"mod"|"rem") ~ p200) ^^ assoc
//
//  def p200: Matcher[TermAST] =
//    p0 ~ pos ~ "**" ~ p0 ^^ mkterm |
//    p0 ~ pos ~ "^" ~ p200 ^^ mkterm |
//    pos ~ ("-"|"\\") ~ p200 ^^ mkterm |
//    p0
//
//  def mklist( ts: List[TermAST], tl: Option[TermAST] ): TermAST =
//    ts match {
//      case Nil if tl isDefined => tl get
//      case Nil => AtomAST( null, "[]" )
//      case head :: tail => StructureAST( head.pos, ".", List(head, mklist(tail, tl)) )
//    }
//
//  private def escape( s: String ) = {
//    val buf = new StringBuilder
//
//    def chr( r: Reader ) {
//      if (!r.eoi) {
//        if (r.ch == '\\') {
//          if (r.next.eoi)
//            sys.error( "unexpected end of string" )//todo: nicer error reporting; not easy - will have to return a special "error" object
//
//          if (r.next.ch == 'u') {
//            var u = r.next.next
//
//            def nextc =
//              if (u.eoi)
//                sys.error( "unexpected end of string inside unicode sequence" )
//              else {
//                val res = u.ch
//
//                u = u.next
//                res
//              }
//
//            buf append Integer.valueOf( new String(Array(nextc, nextc, nextc, nextc)), 16 ).toChar
//            chr( u )
//          } else {
//            buf.append(
//              Map (
//                '\\' -> '\\', '\'' -> '\'', '"' -> '"', '$' -> '$', '/' -> '/', 'b' -> '\b', 'f' -> '\f', 'n' -> '\n', 'r' -> '\r', 't' -> '\t'
//              ).get(r.next.ch) match {
//                case Some( c ) => c
//                case _ => sys.error( "illegal escape character " + r.next.ch )
//              } )
//
//            chr( r.next.next )
//          }
//        } else {
//          buf append r.ch
//          chr( r.next )
//        }
//      }
//    }
//
//    chr( new StringReader(s) )
//    buf.toString()
//  }
//
//  def p0 =
//    pos <~ "!" ^^ (AtomAST( _, "!")) |
//    pos ~ floatLit ^^ { case p ~ n => FloatAST( p, n ) } |
//    pos ~ integerLit ^^ { case p ~ n => IntegerAST( p, n ) } |
//    pos ~ "[" ~ rep1sep(p900, ",") ~ opt("|" ~> p900) ~ "]" ^^ {
//      case p ~ _ ~ ts ~ tl ~ _ => mklist( ts, tl ) } |
//    pos <~ "[" ~ "]" ^^ (AtomAST( _, "[]" )) |
//    "(" ~> term <~ ")" |
//    pos ~ (singleStringLit | identOrReserved) ~ "(" ~ rep1sep(p900, ",") ~ ")" ^^ {
//      case p ~ n ~ _ ~ a ~ _ => StructureAST( p, escape(n), a ) } |
//    pos ~ identOrReserved ^^ {
//      case p ~ "_" => AnonymousAST( p )
//      case p ~ a if a.head.isLower => AtomAST( p, a )
//      case p ~ a => VariableAST( p, a ) } |
//    pos ~ singleStringLit ^^ {
//      case p ~ a => AtomAST( p, escape(a) ) } |
//    pos ~ doubleStringLit ^^ {
//      case p ~ s => StringAST( p, escape(s) ) }
////    pos ~ backStringLit ^^ {
////      case p ~ s => StringAST( p, s ) }
//
//}