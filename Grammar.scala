

/*
	This .sc file contains a parser for the grammar file containing the
	syntax definition of a user-defined programming language. The output
	of this parser would be an abstract syntax tree that would be further
	processed with the ultimate goal of generating a lexical analyzer and
	a parser for the aforementioned programming language.
*/


package jucheparse


object Grammar {


// START OF LEXER


val KEYS1 = (("grammar" | "with" | "generate") | ("program" | "import" | "as"))

val KEYS2 = ("INT" | "DOUBLE" |"STRING")

// KEYS2 may also have CHAR, BOOLEAN, ASCII

val KEYS3 = (("rule" | "enumerate" | "terminal") | ("returns" | "current" | "hidden") | ("abstract" | "component"))


// val KEYS4 = ONE

// val KEYS5 = ONE


val KEYS = (KEYS3 | KEYS2 | KEYS1)


val LETTER = RANGE((('a' to 'z') ++ ('A' to 'Z')).toSet)

val NUMBER = RANGE('0' to '9')


val IDENTIFIER = (LETTER ~ STAR(CHAR('_') | LETTER | NUMBER))

val OPC = RANGE(Set('+', '-', '*', '/', '%', '=', '>', '<', '.', '_', ',', '\\', '!', '?', '|', '&', '~','$', '#', '^', '`', '@'))

val OPS = ((RANGE(Set('+', '=', '!', '<', '>', '?')) ~ CHAR('=')) | "&&" | "||")

val OP = (OPS | OPC)



val BRACKET = RANGE(Set('(', ')', '{', '}', '[', ']'))

val COLON = RANGE(Set(':', ';'))

val Q = RANGE(Set('\"', '\''))



val SYMBOL = (LETTER | NUMBER | OPC | BRACKET | COLON)

val INT = (OPT(CHAR('-')) ~ (CHAR('0') | (RANGE('1' to '9') ~ NUMBER.%)))

val DOUBLE = (INT ~ CHAR('.') ~ (PLUS(NUMBER)))


val WHITESPACE = PLUS(RANGE(Set(' ', '\n', '\t', '\r')))

val STRING = (CHAR('\"') ~ (SYMBOL | WHITESPACE | "\\\"" | "\\\'").% ~ CHAR('\"'))

val COMMENT = ("//" ~ STAR(SYMBOL | RANGE(Set(' ', '\"', '\'', '\t', '\r', '\"', '\''))) ~ "\n")

val CHARACTER = (CHAR('\'') ~ (SYMBOL | RANGE(Set(' ', '\n', '\t', '\r')) | ("\\\"" | "\\\'" | "\\\n" | "\\\t" | "\\\r")) ~ CHAR('\''))




val GRAMMAR_LANG =	STAR(("key" $ KEYS) |
                 		("id" $ IDENTIFIER) |
 										("op" $ OP) |
	                  ("int" $ INT) |
  	                ("db" $ DOUBLE) |
  	                ("str" $ STRING) |
  	                ("char" $ CHARACTER) |
  	                ("space" $ WHITESPACE) |
  	                ("brac" $ BRACKET) |
  	                ("colon" $ COLON) |
  	                ("com" $ COMMENT))



abstract class Token 


case class T_KEY(s: String) extends Token
case class T_ID(s: String) extends Token
case class T_OP(s: String) extends Token
case class T_INT(n: Int) extends Token
case class T_DB(d: Double) extends Token
case class T_STR(s: String) extends Token
case class T_CHAR(c: Char) extends Token
case class T_BRAC(c: Char) extends Token
case class T_COLON(c: Char) extends Token




val token : PartialFunction[(String, String), Token] = {
	case ("key", s) => T_KEY(s)
	case ("id", s) => T_ID(s)
	case ("op", s) => T_OP(s)
	case ("int", s) => T_INT(s.toInt)
	case ("db", s) => T_DB(s.toDouble)
	case ("str", s) => T_STR(s.filter(c => c != '\"'))
	case ("char", s) => try {
		T_CHAR(s.filter(c => c != '\'').replace("\\", "").head)
	} catch {
		case e: Exception => T_CHAR(' ')
	}
	case ("brac", s) => T_BRAC(s.head)
	case ("colon", s) => T_COLON(s.head)
}


// by using collect we filter out all unwanted tokens
def tokenize(s: String) : List[Token] = 
  lex(GRAMMAR_LANG, s).collect(token)



// END OF LEXER



// START OF PARSER



case class ~[+A, +B](x: A, y: B)

// constraint for the input
type IsSeq[A] = A => Seq[_]

abstract class Parser[I : IsSeq, T]{
  def parse(in: I): Set[(T, I)]

  def parse_all(in: I) : Set[T] =
    for ((hd, tl) <- parse(in); 
        if tl.isEmpty) yield hd
}

case class TKP(t: Token) extends Parser[List[Token], Token] {
  def parse(in: List[Token]) = 
    if (in == Nil) Set()
    else if (in.head == t) Set((t, in.tail))
    else Set()
}


// parser combinators

// sequence parser
class SeqParser[I : IsSeq, T, S](p: => Parser[I, T], 
                                 q: => Parser[I, S]) extends Parser[I, ~[T, S]] {
  def parse(in: I) = 
    for ((hd1, tl1) <- p.parse(in); 
         (hd2, tl2) <- q.parse(tl1)) yield (new ~(hd1, hd2), tl2)
}

// alternative parser
class AltParser[I : IsSeq, T](p: => Parser[I, T], 
                              q: => Parser[I, T]) extends Parser[I, T] {
  def parse(in: I) = p.parse(in) ++ q.parse(in)   
}

// map parser
class MapParser[I : IsSeq, T, S](p: => Parser[I, T], 
                                 f: T => S) extends Parser[I, S] {
  def parse(in: I) = for ((hd, tl) <- p.parse(in)) yield (f(hd), tl)
}

/*
// atomic parser for (particular) strings
case class StrParser(s: String) extends Parser[String, String] {
  def parse(sb: String) = {
    val (prefix, suffix) = sb.splitAt(s.length)
    if (prefix == s) Set((prefix, suffix)) else Set()
  }
}
*/

// atomic parser for identifiers (variable names)
case object IdParser extends Parser[List[Token], String] {
  def parse(tl: List[Token]) = 
  	if (tl != Nil) tl match {
  		case T_ID(s) :: ts => Set((s, ts))
  		case _ => Set()
  	}
  	else Set()
}

case object TypeParser extends Parser[List[Token], String] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_KEY("INT") :: ts => Set(("INT", ts))
			case T_KEY("DOUBLE") :: ts => Set(("DOUBLE", ts))
			case T_KEY("STRING") :: ts => Set(("STRING", ts))
			case _ => Set()
		}
		else Set()
	}
}

// atomic parser for ints (transformed into ints)
case object IntParser extends Parser[List[Token], Int] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_INT(n) :: ts => Set((n, ts))
			case _ => Set()
		}
		else Set()
	}
}

case object DoubleParser extends Parser[List[Token], Double] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_DB(db) :: ts => Set((db, ts))
			case _ => Set()
		}
		else Set()
	}
}

// atomic parser for strings (without double quotes)
case object StrParser extends Parser[List[Token], String] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_STR(s) :: ts => Set((s, ts))
			case _ => Set()
		}
		else Set()
	}
}

case object CharParser extends Parser[List[Token], Int] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_CHAR(c) :: ts => Set((c.toInt, ts))
			case _ => Set()
		}
		else Set()
	}
}

case class BracParser(c: Char) extends Parser[List[Token], Char] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_BRAC(br) :: ts => if(br == c) Set((c, ts)) else Set()
			case _ => Set()
		}
		else Set()
	}
}

case class ColonParser(c: Char) extends Parser[List[Token], Char] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_COLON(col) :: ts => if(col == c) Set((c, ts)) else Set()
			case _ => Set()
		}
		else Set()
	}
}

case object AssignParser extends Parser[List[Token], String] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_OP("+=") :: ts => Set(("+=", ts))
			case T_OP("=") :: ts => Set(("=", ts))
			case T_OP("?=") :: ts => Set(("?=", ts))
			case _ => Set()
		}
		else Set()
	}
}


// the following string interpolation allows us to write 
// StrParser(_some_string_) more conveniently as 
//
// p"<_some_string_>" 

/*
implicit def parser_interpolation(sc: StringContext) = new {
    def p(args: Any*) = StrParser(sc.s(args:_*))
}    
*/

// more convenient syntax for parser combinators
implicit def ParserOps[I : IsSeq, T](p: Parser[I, T]) = new {
  def ||(q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
  def map[S](f: => T => S) = new MapParser[I, T, S](p, f)
}



// the abstract syntax trees for the grammar language


abstract class Elem
abstract class Exp
abstract class Stmt


case class Heading(kwd: String, p1: String, p2: String) extends Stmt


case class Program(id: String, exps: List[Exp]) extends Stmt
case class Rule(id: String, exps: List[Exp], mod: Modifier) extends Stmt
case class Enumerate(id: String, el: List[Elem], mod: Modifier) extends Stmt
case class Terminal(id: String, pat: Rexp, mod: Modifier) extends Stmt


case class Keyword(s: String) extends Exp
case class Assign(id: String, op: String, v: Exp) extends Exp
case class CallRule(r: String) extends Exp
case class AltExp(e1: Exp, e2: Exp) extends Exp
case class SeqExp(e1: Exp, e2: Exp) extends Exp
case class RefExp(r: String) extends Exp
case class TypeExp(t: String) extends Exp
case class CardiExp(e: Exp, c: Cardi) extends Exp
case class Action(i: String) extends Exp


// case class IElem(n: String, v: Int) extends Elem
// case class DElem(n: String, v: Double) extends Elem
case class SElem(n: String, v: String) extends Elem


/*
abstract class Type
case object IntType extends Type
case object DoubleType extends Type
case object StringType extends Type
case object CharType extends Type
case class TerminalType(t: Terminal) extends Type
*/


abstract class Cardi
case object OptCardi extends Cardi
case object PlusCardi extends Cardi
case object StarCardi extends Cardi


case class Modifier(component: Boolean, returns: String, hidden: List[String])

def ModP1(m: Modifier) : Boolean = m match {case Modifier(b, _, _) => b ; case _ => false}
def ModP2(m: Modifier) : String = m match {case Modifier(_, s, _) => s ; case _ => ""}
def ModP3(m: Modifier) : List[String] = m match {case Modifier(_, _, sl) => sl ; case _ => Nil}



case object CardiParser extends Parser[List[Token], Cardi] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_OP("*") :: ts => Set((StarCardi, ts))
			case T_OP("+") :: ts => Set((PlusCardi, ts))
			case T_OP("?") :: ts => Set((OptCardi, ts))
			case _ => Set()
		}
		else Set()
	}
}


// case class Terminal and its parser are currently stubs


lazy val Stmt: Parser[List[Token], Stmt] = {
	(TKP(T_KEY("rule")) ~ IdParser ~ Mod ~ Block ~ BracParser('}') ~ ColonParser(';')).map[Stmt]{case _ ~ id ~ m ~ es ~ _ ~ _ => Rule(id, es, m)} ||
	(TKP(T_KEY("enumerate")) ~ IdParser ~ Mod ~ Enum ~ BracParser('}') ~ ColonParser(';')).map[Stmt]{case _ ~ id ~ m ~ en ~ _ ~ _ => Enumerate(id, en, m)} ||
	(TKP(T_KEY("terminal")) ~ IdParser ~ Mod ~ Enum ~ BracParser('}') ~ ColonParser(';')).map[Stmt]{case _ ~ id ~ m ~ en ~ _ ~ _ => Terminal(id, ONE, m) } ||
	(TKP(T_KEY("program")) ~ IdParser ~ BracParser('{') ~ Block ~ BracParser('}') ~ ColonParser(';')).map[Stmt]{case _ ~ id ~ _ ~ es ~ _ ~ _ => Program(id, es)} ||
	Head
}



lazy val Mod: Parser[List[Token], Modifier] = {
	(TKP(T_KEY("component")) ~ Mod).map[Modifier]{case _ ~ ms => Modifier(true, ModP2(ms), ModP3(ms))} ||
	(TKP(T_KEY("returns")) ~ IdParser ~ Mod).map[Modifier]{case _ ~ id ~ ms => Modifier(ModP1(ms), id, ModP3(ms))} ||
	(TKP(T_KEY("hidden")) ~ BracParser('(') ~ Hiddens ~ BracParser(')') ~ Mod).map[Modifier]{case _ ~ _ ~ hs ~ _ ~ ms => Modifier(ModP1(ms), ModP2(ms), hs)} ||
	(BracParser('{')).map[Modifier]{_ => Modifier(false, "", Nil)}
}


lazy val Hiddens: Parser[List[Token], List[String]] = {
	(IdParser ~ TKP(T_OP(",")) ~ Hiddens).map{case h ~ _ ~ hs => h :: hs} ||
	(IdParser).map{h => List(h)}
}


lazy val Head: Parser[List[Token], Stmt] = {
	(TKP(T_KEY("grammar")) ~ Path ~ ColonParser(';')).map[Stmt]{case _ ~ p1 ~ _ => Heading("grammar", p1, "")} ||
	(TKP(T_KEY("grammar")) ~ Path ~ TKP(T_KEY("with")) ~ Path ~ ColonParser(';')).map[Stmt]{case _ ~ p1 ~ _ ~ p2 ~ _ => Heading("grammar", p1, p2)} ||
	(TKP(T_KEY("generate")) ~ IdParser ~ StrParser ~ ColonParser(';')).map[Stmt]{case _ ~ o ~ s ~ _ => Heading("generate", o, s)} ||
	(TKP(T_KEY("import")) ~ StrParser ~ ColonParser(';')).map[Stmt]{case _ ~ s ~ _ => Heading("import", s, "")} ||
	(TKP(T_KEY("import")) ~ StrParser ~ TKP(T_KEY("as")) ~ IdParser ~ ColonParser(';')).map[Stmt]{case _ ~ s ~ _ ~ id ~ _ => Heading("import", s, id)}
}


lazy val Path: Parser[List[Token], String] = {
	(IdParser ~ TKP(T_OP(".")) ~ Path).map[String]{case id ~ _ ~ ps => id ++ ps} ||
	IdParser.map[String]{a => a}
}


lazy val Exp: Parser[List[Token], Exp] = {
	(StrParser).map[Exp]{case s => Keyword(s)} ||
	(IdParser ~ AssignParser ~ Exp).map[Exp]{case id ~ o ~ v => Assign(id, o, v)} ||
  (IdParser).map[Exp]{case r => CallRule(r)} ||
  (BracParser('(') ~ Exp ~ BracParser(')') ~ CardiParser).map[Exp]{case _ ~ e ~ _ ~ c => CardiExp(e, c)} ||
	(BracParser('[') ~ IdParser ~ BracParser(']')).map[Exp]{case _ ~ r ~ _ => RefExp(r)} ||
  (BracParser('(') ~ Exp ~ BracParser(')')).map[Exp]{case _ ~ e ~ _ => e}
}


lazy val Block: Parser[List[Token], List[Exp]] = {
	(Line ~ TKP(T_OP(",")) ~ Block).map[List[Exp]]{case l ~ _ ~ b => l :: b} ||
	Line.map[List[Exp]]{l => List(l)}
}


lazy val Line: Parser[List[Token], Exp] = {
  (AltDef).map[Exp]{al => al} ||
  (SeqDef).map[Exp]{sq => sq} ||
  Exp
}


lazy val AltDef: Parser[List[Token], Exp] = {
	(Exp ~ TKP(T_OP("|")) ~ AltDef).map[Exp]{case e ~ _ ~ al => AltExp(e, al)} ||
	Exp
}


lazy val SeqDef: Parser[List[Token], Exp] = {
	(Exp ~ SeqDef).map[Exp]{case e ~ sq => SeqExp(e, sq)} ||
	Exp
}


lazy val Enum: Parser[List[Token], List[Elem]] = {
	(Elem ~ TKP(T_OP("|")) ~ Enum).map{case e ~ _ ~ en => e :: en} ||
	Elem.map{e => List(e)}
}


lazy val Elem: Parser[List[Token], Elem] = {
  (IdParser ~ TKP(T_OP("=")) ~ StrParser).map[Elem]{case i ~ _ ~ v => SElem(i, v)} ||
  (StrParser).map[Elem]{s => SElem(s, s)} ||
  (IdParser).map[Elem]{i => SElem(i, i)}
}



// lazy val Pattern: Parser[List[Token], Rexp] = {}


/*

// arithmetic expressions
lazy val AExp: Parser[List[Token], Exp] = {
  (Term ~ TKP(T_AOP("+")) ~ AExp).map[Exp]{ case x ~ _ ~ z => Aop("+", x, z) } ||
  (Term ~ TKP(T_AOP("-")) ~ AExp).map[Exp]{ case x ~ _ ~ z => Aop("-", x, z) } || 
  Term
}
  
lazy val Term: Parser[List[Token], Exp] = {
  (Factor ~ TKP(T_AOP("*")) ~ Term).map[Exp]{ case x ~ _ ~ z => Aop("*", x, z) } || 
  (Factor ~ TKP(T_AOP("/")) ~ Term).map[Exp]{ case x ~ _ ~ z => Aop("/", x, z) } ||
  (Factor ~ TKP(T_AOP("%")) ~ Term).map[Exp]{ case x ~ _ ~ z => Aop("%", x, z) } || 
  Factor
} 
  
lazy val Factor: Parser[List[Token], Exp] = {
   (BracParser('(') ~ AExp ~ BracParser(')')).map{ case _ ~ y ~ _ => y } ||
   CallParser.map[Exp]{c => c} ||
   IdParser.map(Var) || IntParser.map(Num) || DoubleParser.map(FNum) ||
   CharParser.map(ChConst)
}

*/


lazy val Grammar: Parser[List[Token], List[Stmt]] = {
	(Stmt ~ Grammar).map{case s ~ g => s :: g} ||
	Stmt.map{s => List(s)}
}


def lex(code: String) : List[Token] = tokenize(code)

def parse(code: String) : List[Stmt] = Grammar.parse_all(tokenize(code)).head

def parse_tokens(tl: List[Token]) : List[Stmt] = Grammar.parse_all(tl).head



// END OF PARSER


/*

Consider additional functionalities:
1. Terminal rules with regular expressions
2. Actions

*/


}



