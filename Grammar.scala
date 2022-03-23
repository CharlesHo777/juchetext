
// package jucheparse

/*
	This .scala file contains the parser for the grammar file containing the syntax definitions of a user-defined programming language. The output of this parser would be an abstract syntax tree that would be further processed with the ultimate goal of generating a lexical analyzer and a parser for the aforementioned programming language.
*/

object Grammar {

// Lexer & Parser For The Grammar Language

val KEYS = ("rule" | "enumerate" | "terminal" | "returns" | "current" | "hidden" | "abstract" | "fragment" | "ID" | "INT" | "DOUBLE" | "STRING" | "CHAR" | "grammar" | "program" | "name" | "t")

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

case object SpecialParser extends Parser[List[Token], Char] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_SP("\\w") :: ts => Set((' ', ts))
			case T_SP("\\n") :: ts => Set(('\n', ts))
			case T_SP("\\t") :: ts => Set(('\t', ts))
			case T_SP("\\r") :: ts => Set(('\r', ts))
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
case class Rule(id: String, exps: List[Exp], ret: String) extends Stmt
case class Enumerate(id: String, el: List[Elem], ret: String) extends Stmt
case class Terminal(id: String, pat: Rexp, ret: String) extends Stmt

case class Keyword(s: String) extends Exp
case class CallRule(r: String) extends Exp
case class AltExp(e1: Exp, e2: Exp) extends Exp
case class Name(e: Exp) extends Exp
case class RefExp(r: String) extends Exp
case class TypeExp(t: String) extends Exp
case class CardiExp(e: Exp, c: Cardi) extends Exp
case class Action(i: String) extends Exp

case object NewLine extends Exp
case object NoSpace extends Exp
case class AddSpace(ws: Char) extends Exp
case class MultiSpace(ws: Char, min: Int, max: Int) extends Exp
case object IncTab extends Exp
case object DecTab extends Exp
case object SameTab extends Exp

case class IElem(v: Int) extends Elem // Int
case class DElem(v: Double) extends Elem // Double
case class SElem(v: String) extends Elem // String

abstract class Cardi
case object C_OPT extends Cardi
case object C_PLUS extends Cardi
case object C_STAR extends Cardi
case object C_HID extends Cardi

case object CardiParser extends Parser[List[Token], Cardi] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_OP('*') :: ts => Set((C_OPT, ts))
			case T_OP('+') :: ts => Set((C_PLUS, ts))
			case T_OP('?') :: ts => Set((C_STAR, ts))
			case T_OP('/') :: ts => Set((C_HID, ts))
			case _ => Set()
		}
		else Set()
	}
}

lazy val Stmt: Parser[List[Token], Stmt] = {
	(TKP(T_KEY("rule")) ~ Heading ~ BracParser('{') ~ Block ~ BracParser('}')).map[Stmt]{
		case _ ~ hd ~ _ ~ es ~ _ => Rule(hd._1, es, hd._2)
	} ||
	(TKP(T_KEY("enumerate")) ~ Heading ~ BracParser('{') ~ Enum ~ BracParser('}')).map[Stmt]{
		case _ ~ hd ~ _ ~ en ~ _ => Enumerate(hd._1, en, hd._2)
	} ||
	(TKP(T_KEY("terminal")) ~ Heading ~ BracParser('{') ~ Pattern ~ BracParser('}')).map[Stmt]{
		case _ ~ hd ~ _ ~ p ~ _ => Terminal(hd._1, p, hd._2)
	} ||
	(TKP(T_KEY("program")) ~ IdParser ~ BracParser('{') ~ Block ~ BracParser('}')).map[Stmt]{
		case _ ~ id ~ _ ~ es ~ _ => Program(id, es)
	} ||
	(TKP(T_KEY("grammar")) ~ IdParser).map[Stmt]{
		case _ ~ t => Title(t)
	}
}

lazy val Heading: Parser[List[Token], (String, String)] = {
	(IdParser ~ TKP(T_KEY("returns")) ~ IdParser).map{
		case id ~ _ ~ rt => (id, rt)
	} ||
	(IdParser).map{
		id => (id, "")
	}
}

/*
lazy val Path: Parser[List[Token], String] = {
	(IdParser ~ TKP(T_OP(".")) ~ Path).map[String]{
		case id ~ _ ~ ps => id ++ ps
	} ||
	IdParser.map[String]{a => a}
}
*/

lazy val Exp: Parser[List[Token], Exp] = {
	(StrParser).map[Exp]{
		case s => Keyword(s)
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
	(Spacing ~ Block).map[List[Exp]]{
		case _ ~ b => NewLine :: b
	} ||
	(Exp || AltDef).map[List[Exp]]{
		e => List(e)
	}
}

lazy val Spacing: Parser[List[Token], Exp] = {
	(OpParser(';')).map[Exp]{
		_ => NewLine
	} ||
	(SpecialParser).map[Exp]{
		c => AddSpace(c)
	} ||
	(SpecialParser ~ BracParser('{') ~ IntParser ~ BracParser('}')).map[Exp]{
		case c ~ _ ~ n ~ _ => MultiSpace(c, n, n)
	} ||
	(SpecialParser ~ BracParser('{') ~ IntParser ~ OpParser(',') ~ IntParser ~ BracParser('}')).map[Exp]{
		case c ~ _ ~ min ~ _ ~ max ~ _ => MultiSpace(c, min, max)
	} ||
	(TKP(T_KEY("t")) ~ OpParser('+')).map[Exp]{
		case _ ~ _ => IncTab
	} ||
	(TKP(T_KEY("t")) ~ OpParser('-')).map[Exp]{
		case _ ~ _ => DecTab
	} ||
	(TKP(T_KEY("t"))).map[Exp]{
		_ => SameTab
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

// Consider adding Actions

// END OF FILE Grammar.scala
