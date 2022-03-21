
// package jucheparse

/*
	This .scala file contains the parser for the grammar file containing the syntax definitions of a user-defined programming language. The output of this parser would be an abstract syntax tree that would be further processed with the ultimate goal of generating a lexical analyzer and a parser for the aforementioned programming language.
*/

object Grammar {

// Lexer & Parser For The Grammar Language

val KEYS = ("rule" | "enumerate" | "terminal" | "returns" | "current" | "hidden" | "abstract" | "fragment" | "ID" | "INT" | "DOUBLE" | "STRING" | "grammar" | "program")

val LETTER = RANGE((('a' to 'z') ++ ('A' to 'Z')).toSet)

val NUMBER = RANGE('0' to '9')

val IDENTIFIER = (LETTER ~ STAR(CHAR('_') | LETTER | NUMBER))

val OPC = RANGE(Set('+', '-', '*', '/', '%', '=', '>', '<', '.', '_', ',', '\\', '!', '?', '|', '&', '~','$', '#', '^', '`', '@'))

val OPS = ((RANGE(Set('+', '=', '!', '<', '>', '?')) ~ CHAR('=')) | "&&" | "||")

val OP = (OPS | OPC)

val BRACKET = RANGE(Set('(', ')', '{', '}', '[', ']'))

val COLON = RANGE(Set(':', ';'))

val QUOTE = RANGE(Set('\"', '\''))

val SYMBOL = (LETTER | NUMBER | OPC | BRACKET | COLON)

val INT = (OPT(CHAR('-')) ~ (CHAR('0') | (RANGE('1' to '9') ~ NUMBER.%)))

val DOUBLE = (INT ~ CHAR('.') ~ (PLUS(NUMBER)))

val WHITESPACE = PLUS(RANGE(Set(' ', '\n', '\t', '\r')))

val STRING = (CHAR('\"') ~ (SYMBOL | WHITESPACE | "\\\"" | "\\\'").% ~ CHAR('\"'))

val COMMENT = ("//" ~ STAR(SYMBOL | RANGE(Set(' ', '\"', '\'', '\t', '\r', '\"', '\''))) ~ "\n")

val WSCHAR = (CHAR('\'') ~ (
		CHAR(' ') | ( CHAR('\\') ~ RANGE(Set('n', 't', 'r')) )
	) ~ CHAR('\'')
)

val GRAMMAR_LANG = {
	STAR(
		("key" $ KEYS) |
		("id" $ IDENTIFIER) |
		("op" $ OP) |
		("int" $ INT) |
		("db" $ DOUBLE) |
		("str" $ STRING) |
		("char" $ WSCHAR) |
		("space" $ WHITESPACE) |
		("brac" $ BRACKET) |
		("colon" $ COLON) |
		("com" $ COMMENT)
	)
}

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
			case T_KEY("ID") :: ts => Set(("ID", ts))
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

// the abstract syntax trees for the grammar language

abstract class Elem
abstract class Exp
abstract class Stmt

case class Title(p: String) extends Stmt

case class Program(id: String, exps: List[Exp]) extends Stmt
case class Rule(id: String, exps: List[Exp], mod: Modifier) extends Stmt
case class Enumerate(id: String, el: List[Elem], mod: Modifier) extends Stmt
case class Terminal(id: String, pat: Rexp, mod: Modifier, fragment: Boolean) extends Stmt

case class Keyword(s: String) extends Exp
case class Assign(id: String, op: String, v: Exp) extends Exp
case class CallRule(r: String) extends Exp
case class AltExp(e1: Exp, e2: Exp) extends Exp
case class SeqExp(e1: Exp, e2: Exp) extends Exp
case class RefExp(r: String) extends Exp
case class TypeExp(t: String) extends Exp
case class CardiExp(e: Exp, c: Cardi) extends Exp
case class Action(i: String) extends Exp
case class WS(c: Char, n: Int) extends Exp
case object NewLine extends Exp

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

case class Modifier(returns: String, hidden: List[String])

def ModP1(m: Modifier) : String = m match {
	case Modifier(s, _) => s
	case _ => ""
}
def ModP2(m: Modifier) : List[String] = m match {
	case Modifier(_, sl) => sl
	case _ => Nil
}

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

lazy val Stmt: Parser[List[Token], Stmt] = {
	(TKP(T_KEY("rule")) ~ IdParser ~ Mod ~ Block ~ BracParser('}')).map[Stmt]{
		case _ ~ id ~ m ~ es ~ _ => Rule(id, es, m)
	} ||
	(TKP(T_KEY("enumerate")) ~ IdParser ~ Mod ~ Enum ~ BracParser('}')).map[Stmt]{
		case _ ~ id ~ m ~ en ~ _ => Enumerate(id, en, m)
	} ||
	(TKP(T_KEY("terminal")) ~ IdParser ~ Mod ~ Pattern ~ BracParser('}')).map[Stmt]{
		case _ ~ id ~ m ~ p ~ _ => Terminal(id, p, m, false)
	} ||
	(TKP(T_KEY("terminal")) ~ TKP(T_KEY("fragment")) ~ IdParser ~ Mod ~ Pattern ~ BracParser('}')).map[Stmt]{
		case _ ~ _ ~ id ~ m ~ p ~ _ => Terminal(id, p, m, true)
	} ||
	(TKP(T_KEY("program")) ~ IdParser ~ BracParser('{') ~ Block ~ BracParser('}')).map[Stmt]{
		case _ ~ id ~ _ ~ es ~ _ => Program(id, es)
	} ||
	(TKP(T_KEY("grammar")) ~ Path).map[Stmt]{
		case _ ~ p => Title(p)
	}
}

lazy val Mod: Parser[List[Token], Modifier] = {
	(TKP(T_KEY("returns")) ~ IdParser ~ Mod).map[Modifier]{
		case _ ~ id ~ ms => Modifier(id, ModP2(ms))
	} ||
	(TKP(T_KEY("hidden")) ~ BracParser('(') ~ Hiddens ~ BracParser(')') ~ Mod).map[Modifier]{
		case _ ~ _ ~ hs ~ _ ~ ms => Modifier(ModP1(ms), hs)
	} ||
	(BracParser('{')).map[Modifier]{_ => Modifier("", Nil)}
}

lazy val Hiddens: Parser[List[Token], List[String]] = {
	(IdParser ~ TKP(T_OP(",")) ~ Hiddens).map{
		case h ~ _ ~ hs => h :: hs
	} ||
	(IdParser).map{h => List(h)}
}

lazy val Path: Parser[List[Token], String] = {
	(IdParser ~ TKP(T_OP(".")) ~ Path).map[String]{
		case id ~ _ ~ ps => id ++ ps
	} ||
	IdParser.map[String]{a => a}
}

lazy val Exp: Parser[List[Token], Exp] = {
	(StrParser).map[Exp]{
		case s => Keyword(s)
	} ||
	(IdParser ~ AssignParser ~ Exp).map[Exp]{
		case id ~ o ~ v => Assign(id, o, v)
	} ||
  (IdParser).map[Exp]{
		case r => CallRule(r)
	} ||
  (BracParser('(') ~ Exp ~ BracParser(')') ~ CardiParser).map[Exp]{
		case _ ~ e ~ _ ~ c => CardiExp(e, c)
	} ||
	(BracParser('[') ~ IdParser ~ BracParser(']')).map[Exp]{
		case _ ~ r ~ _ => RefExp(r)
	} ||
	(TypeParser).map[Exp]{
		case t => TypeExp(t)
	} ||
  (BracParser('(') ~ Exp ~ BracParser(')')).map[Exp]{
		case _ ~ e ~ _ => e
	}
}

lazy val Block: Parser[List[Token], List[Exp]] = {
	((Exp || AltDef) ~ Block).map[List[Exp]]{
		case e ~ b => e :: b
	} ||
	(ColonParser(';') ~ Block).map[List[Exp]]{
		case _ ~ b => NewLine :: b
	} ||
	(Exp || AltDef).map[List[Exp]]{
		e => List(e)
	}
}

lazy val AltDef: Parser[List[Token], Exp] = {
	(Exp ~ TKP(T_OP("|")) ~ AltDef).map[Exp]{
		case e ~ _ ~ al => AltExp(e, al)
	} ||
	Exp
}

lazy val Enum: Parser[List[Token], List[Elem]] = {
	(Elem ~ TKP(T_OP("|")) ~ Enum).map{
		case e ~ _ ~ en => e :: en
	} ||
	Elem.map{e => List(e)}
}

lazy val Elem: Parser[List[Token], Elem] = {
  (IdParser ~ TKP(T_OP("=")) ~ StrParser).map[Elem]{
		case i ~ _ ~ v => SElem(i, v)
	} ||
  (StrParser).map[Elem]{s => SElem(s, s)} ||
  (IdParser).map[Elem]{i => SElem(i, i)}
}

lazy val Pattern: Parser[List[Token], Rexp] = {
	RegexParser.Reg
}

lazy val Stmts: Parser[List[Token], List[Stmt]] = {
	(Stmt ~ Stmts).map{case s ~ g => s :: g} ||
	Stmt.map{s => List(s)}
}

def parse(code: String) : List[Stmt] = try {
	Stmts.parse_all(tokenize(code)).head
} catch {
	case e: Exception => Nil
}

def parse_tokens(tl: List[Token]) : List[Stmt] = try {
	Stmts.parse_all(tl).head
} catch {
	case e: Exception => Nil
}

// END OF OBJECT Grammar

}

// Consider adding Actions

// END OF FILE Grammar.scala
