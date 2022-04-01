
// package jucheparse

/*
	This .scala file contains the parser for the grammar file containing the syntax definitions of a user-defined programming language. The output of this parser would be an abstract syntax tree that would be further processed with the ultimate goal of generating a lexical analyzer and a parser for the aforementioned programming language.
*/

object Grammar {

// Lexer & Parser For The Grammar Language

val KEYS = ("rule" | "enumerate" | "terminal" | "abstract" | "fragment" | "ID" | "INT" | "DOUBLE" | "STRING" | "CHAR" | "grammar" | "program" | "ignore" | "min" | "max")

val LETTER = RANGE((('a' to 'z') ++ ('A' to 'Z')).toSet)

val DIGIT = RANGE('0' to '9')

val IDENTIFIER = (LETTER ~ STAR(CHAR('_') | LETTER | DIGIT))

val OP = RANGE(Set('+', '-', '*', '/', '%', '=', '>', '<', '.', '_', ',', ';', ':', '!', '?', '|', '&', '~','$', '#', '^', '`', '@'))

// val OPS = ((RANGE(Set('+', '=', '!', '<', '>', '?')) ~ CHAR('=')) | "&&" | "||")

val BRACKET = RANGE(Set('(', ')', '{', '}', '[', ']'))

val QUOTE = RANGE(Set('\"', '\''))

val SYMBOL = ((LETTER | DIGIT) | (OP | BRACKET))

val INT = (OPT(CHAR('-')) ~ (CHAR('0') | (RANGE('1' to '9') ~ DIGIT.%)))

val DOUBLE = (INT ~ CHAR('.') ~ (PLUS(DIGIT)))

val WS = RANGE(Set(' ', '\n', '\t', '\r'))

val WHITESPACE = PLUS(WS)

val ESCAPED = (CHAR('\\') ~ RANGE(Set('\\', '\"', '\'', 'n', 't', 'r')))

val STRING = (CHAR('\"') ~ STAR(SYMBOL | WS | ESCAPED | '\'') ~ CHAR('\"'))

val CHARACTER = (CHAR('\'') ~ (SYMBOL | WS | ESCAPED | '\"') ~ CHAR('\''))

val COMMENT = ("//" ~ STAR(SYMBOL | RANGE(Set(' ', '\"', '\'', '\t', '\r', '\"', '\''))) ~ "\n")

val SPECIAL = ("\\w" | "\\n" | "\\t" | "\\r")

val GRAMMAR_LANG = {
	STAR(
		(("key" $ KEYS) | ("id" $ IDENTIFIER) | ("op" $ OP)) |
		(("int" $ INT) | ("db" $ DOUBLE) | ("str" $ STRING)) |
		(("brac" $ BRACKET) | ("sp" $ SPECIAL)) |
		(("space" $ WHITESPACE) | ("com" $ COMMENT))
	)
}

case class T_KEY(s: String) extends Token
case class T_ID(s: String) extends Token
case class T_OP(c: Char) extends Token
case class T_INT(n: Int) extends Token
case class T_DB(d: Double) extends Token
case class T_STR(s: String) extends Token
case class T_BRAC(c: Char) extends Token
case class T_SP(s: String) extends Token

def process_string(s: List[Char]) : List[Char] = s match {
	case '\\' :: '\\' :: cs => '\\' :: process_string(cs)
	case '\\' :: '\"' :: cs => '\"' :: process_string(cs)
	case '\\' :: '\'' :: cs => '\'' :: process_string(cs)
	case '\\' :: 'n' :: cs => '\n' :: process_string(cs)
	case '\\' :: 't' :: cs => '\t' :: process_string(cs)
	case '\\' :: 'r' :: cs => '\r' :: process_string(cs)
	case c :: cs => c :: process_string(cs)
	case Nil => Nil
}

val token : PartialFunction[(String, String), Token] = {
	case ("key", s) => T_KEY(s)
	case ("id", s) => T_ID(s)
	case ("op", s) =>
		try {T_OP(s.head)}
		catch {case e: Exception => T_OP(' ')}
	case ("int", s) =>
		try {T_INT(s.toInt)}
		catch {case e: Exception => T_INT(0)}
	case ("db", s) =>
		try {T_DB(s.toDouble)}
		catch {case e: Exception => T_DB(0.0)}
	case ("str", s) =>
		try {
			val s2 = s.init.tail
			val s3 = process_string(s2.toList).mkString
			T_STR(s3)
		} catch {
			case e: Exception => T_STR("")
		}
	case ("brac", s) => 
		try {T_BRAC(s.head)}
		catch {case e: Exception => T_BRAC(' ')}
	case ("sp", s) => T_SP(s)
}

// by using collect we filter out all unwanted tokens
def tokenize(s: String) : List[Token] = {
  lex(GRAMMAR_LANG, s).collect(token)
}

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
			case T_KEY("CHAR") :: ts => Set(("CHAR", ts))
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
			case T_BRAC(br) :: ts if (br == c) => Set((c, ts))
			case _ => Set()
		}
		else Set()
	}
}

case class OpParser(op: Char) extends Parser[List[Token], Char] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_OP(c) :: ts if (c == op) => Set((c, ts))
			case _ => Set()
		}
		else Set()
	}
}

// the abstract syntax trees for the grammar language

abstract class Stmt
abstract class Exp
abstract class Elem

case class Title(p: String) extends Stmt
case class Program(id: String, exps: List[Exp]) extends Stmt
case class Rule(id: String, exps: List[Exp]) extends Stmt
case class Enumerate(id: String, el: List[Elem]) extends Stmt
case class Terminal(id: String, pat: Rexp, priority: Int, frag: Boolean) extends Stmt {
	def compare(t2: Terminal) : Int = {
		val priority_2 = t2.priority
		if (priority < priority_2) -1
		else if (priority == priority_2) 0
		else 1
	}
}

case class Hidden(id: String, pat: Rexp) extends Stmt

case class Keyword(s: String) extends Exp
case class CallRule(r: String) extends Exp
case class AltExp(e1: Exp, e2: Exp) extends Exp
case class TypeExp(t: String) extends Exp
case class CardiExp(e: Exp, c: Cardi) extends Exp

case class IElem(v: Int) extends Elem // Int
case class DElem(v: Double) extends Elem // Double
case class SElem(v: String) extends Elem // String

abstract class Cardi
case object C_OPT extends Cardi
case object C_PLUS extends Cardi
case object C_STAR extends Cardi
case class C_EXACT(n: Int) extends Cardi
case class C_MIN(min: Int) extends Cardi
case class C_MINMAX(min: Int, max: Int) extends Cardi

case object CardiParser extends Parser[List[Token], Cardi] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_OP('*') :: ts => Set((C_OPT, ts))
			case T_OP('+') :: ts => Set((C_PLUS, ts))
			case T_OP('?') :: ts => Set((C_STAR, ts))
			case _ => Set()
		}
		else Set()
	}
}

lazy val Stmt: Parser[List[Token], Stmt] = {
	(TKP(T_KEY("rule")) ~ IdParser ~ BracParser('{') ~ Block ~ BracParser('}')).map[Stmt]{
		case _ ~ id ~ _ ~ es ~ _ => Rule(id, es)
	} ||
	(TKP(T_KEY("enumerate")) ~ IdParser ~ BracParser('{') ~ Enum ~ BracParser('}')).map[Stmt]{
		case _ ~ id ~ _ ~ en ~ _ => Enumerate(id, en)
	} ||
	(TerminalParser) ||
	(TKP(T_KEY("program")) ~ IdParser ~ BracParser('{') ~ Block ~ BracParser('}')).map[Stmt]{
		case _ ~ id ~ _ ~ es ~ _ => Program(id, es)
	} ||
	(TKP(T_KEY("grammar")) ~ IdParser).map[Stmt]{
		case _ ~ t => Title(t)
	}
}

lazy val TerminalParser: Parser[List[Token], Stmt] = {
	(TKP(T_KEY("terminal")) ~ IdParser ~ BracParser('{') ~ Pattern ~ BracParser('}')).map[Stmt]{
		case _ ~ id ~ _ ~ p ~ _ => Terminal(id, p, 0, false)
	} ||
	(TKP(T_KEY("terminal")) ~ IntParser ~ IdParser ~ BracParser('{') ~ Pattern ~ BracParser('}')).map[Stmt]{
		case _ ~ pr ~ id ~ _ ~ p ~ _ => Terminal(id, p, pr, false)
	} ||
	(TKP(T_KEY("terminal")) ~ TKP(T_KEY("fragment")) ~ IdParser ~ BracParser('{') ~ Pattern ~ BracParser('}')).map[Stmt]{
		case _ ~ _ ~ id ~ _ ~ p ~ _ => Terminal(id, p, 0, true)
	} ||
	(OpParser('@') ~ TKP(T_KEY("ignore")) ~ TKP(T_KEY("terminal")) ~ IdParser ~ BracParser('{') ~ Pattern ~ BracParser('}')).map[Stmt]{
		case _ ~ _ ~ _ ~ id ~ _ ~ p ~ _ => Hidden(id, p)
	}
}

lazy val Exp: Parser[List[Token], Exp] = {
	(StrParser).map[Exp]{
		case s => Keyword(s)
	} ||
  (IdParser).map[Exp]{
		case r => CallRule(r)
	} ||
  (BracParser('(') ~ (Exp || AltDef) ~ BracParser(')') ~ CardiParser).map[Exp]{
		case _ ~ e ~ _ ~ c => CardiExp(e, c)
	} ||
	(TypeParser).map[Exp]{
		case t => TypeExp(t)
	} ||
  (BracParser('(') ~ (Exp || AltDef) ~ BracParser(')')).map[Exp]{
		case _ ~ e ~ _ => e
	}
}

lazy val Cardinality: Parser[List[Token], Cardi] = {
	(CardiParser) ||
	(BracParser('{') ~ IntParser ~ BracParser('}')).map[Cardi]{
		case _ ~ n ~ _ => C_EXACT(n)
	} ||
	(BracParser('{') ~ IntParser ~ OpParser(',') ~ IntParser ~ BracParser('}')).map[Cardi]{
		case _ ~ min ~ _ ~ max ~ _ => C_MINMAX(min, max)
	} ||
	(BracParser('{') ~ TKP(T_KEY("min")) ~ IntParser ~ BracParser('}')).map[Cardi]{
		case _ ~ _ ~ min ~ _ => C_MIN(min)
	} ||
	(BracParser('{') ~ TKP(T_KEY("max")) ~ IntParser ~ BracParser('}')).map[Cardi]{
		case _ ~ _ ~ max ~ _ => C_MINMAX(0, max)
	}
}

lazy val Block: Parser[List[Token], List[Exp]] = {
	((Exp || AltDef) ~ Block).map{
		case e ~ b => e :: b
	} ||
	(Exp || AltDef).map{
		e => List(e)
	}
}

lazy val AltDef: Parser[List[Token], Exp] = {
	(Exp ~ OpParser('|') ~ AltDef).map[Exp]{
		case e ~ _ ~ al => AltExp(e, al)
	} ||
	Exp
}

lazy val Enum: Parser[List[Token], List[Elem]] = {
	(Elem ~ OpParser('|') ~ Enum).map{
		case e ~ _ ~ en => e :: en
	} ||
	Elem.map{e => List(e)}
}

lazy val Elem: Parser[List[Token], Elem] = {
  (StrParser || IdParser).map[Elem]{s => SElem(s)} ||
	(IntParser).map[Elem]{n => IElem(n)} ||
	(DoubleParser).map[Elem]{d => DElem(d)}
}

lazy val Pattern: Parser[List[Token], Rexp] = {
	(StrParser).map[Rexp]{
		s => 	try {RegexParser.parse(s)}
					catch {case e: Exception =>
						println("Error when parsing pattern in a terminal rule")
						ZERO
					}
	}
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

// END OF FILE Grammar.scala
