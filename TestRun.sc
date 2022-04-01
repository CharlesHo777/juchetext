
// package jucheparse

// regular expressions including records
abstract class Rexp

case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp {
	override def toString : String = {
		s"""CHAR('${c}')"""
	}
}
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp

case class RANGE(s: Set[Char]) extends Rexp {
	override def toString : String = {
		val s_mod = s.map(c => s"\'${c}\'")
		s"""RANGE(${s_mod})"""
	}
}
case class PLUS(r: Rexp) extends Rexp
case class OPT(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp

case class CHARSEQ(cl: List[Char]) extends Rexp {
	override def toString : String = {
		val cl_mod = cl.map(c => s"\'${c}\'")
		s"""CHARSEQ(${cl_mod})"""
	}
}
case object ANY extends Rexp
case class BOUND(r: Rexp, min: Int, max: Int) extends Rexp
case class NOT(s: Set[Char]) extends Rexp {
	override def toString : String = {
		val s_mod = s.map(c => s"\'${c}\'")
		s"""NOT(${s_mod})"""
	}
}
case class NOTSEQ(cl: List[Char]) extends Rexp {
	override def toString : String = {
		val cl_mod = cl.map(c => s"\'${c}\'")
		s"""NOTSEQ(${cl_mod})"""
	}
}

// records for extracting strings or tokens
case class RECD(x: String, r: Rexp) extends Rexp {
	override def toString : String = {
		val x_mod = s"\"${x}\""
		s"""RECD(${x_mod}, ${r})"""
	}
}

case class MARK(s: String) extends Rexp {
	override def toString : String = {
		s
	}
}

/* def RANGE takes a parameter of type collection.immutable.NumericRange[Char],
the output value is of type Rexp and is equivalent to case class RANGE */
def RANGE(range: collection.immutable.NumericRange[Char]): Rexp = {
	RANGE(range.toSet)
}

// values  
abstract class Val

case object NotMatched extends Val {
	override def toString = "Your regular expression does not match the input string (or the program may be faulty, I really don't know)."
}
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val
case class Rec(x: String, v: Val) extends Val

case class Ranged(c: Char) extends Val
case class More(v: Val, vs: Stars) extends Val
case class Opted(v: Val) extends Val
case class Exact(vs: List[Val], n: Int) extends Val

case class ChrSq(cl: List[Char]) extends Val
case class AnyChar(c: Char) extends Val
case class Bounded(vs: List[Val], n: Int) extends Val
case class Nein(c: Char) extends Val
case class NeinSq(cl: List[Char]) extends Val

abstract class Token 

// some convenience for typing in regular expressions

def charlist2rexp(s: List[Char]): Rexp = s match {
	case Nil => ONE
	case c::Nil => CHAR(c)
	case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

implicit def string2rexp(s: String) : Rexp = {
	CHARSEQ(s.toList)
}

implicit def char2rexp(c: Char) : Rexp = {
	CHAR(c)
}

implicit def RexpOps(r: Rexp) = new {

	def | (s: Rexp) = ALT(r, s)
	def % = STAR(r)
	def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps(s: String) = new {
	def | (r: Rexp) = ALT(s, r)
	def | (r: String) = ALT(s, r)
	def % = STAR(CHARSEQ(s.toList))
	def ~ (r: Rexp) = SEQ(s, r)
	def ~ (r: String) = SEQ(s, r)
	def $ (r: Rexp) = RECD(s, r)
}

implicit def charOps(c: Char) = new {
	def | (r: Rexp) = ALT(c, r)
	def | (r: Char) = ALT(c, r)
	def ~ (r: Rexp) = SEQ(c, r)
	def ~ (r: Char) = SEQ(c, r)
	def % = STAR(CHAR(c))
}

def nullable(r: Rexp) : Boolean = r match {
	case ZERO => false
	case ONE => true
	case CHAR(_) => false
	case ALT(r1, r2) => nullable(r1) || nullable(r2)
	case SEQ(r1, r2) => nullable(r1) && nullable(r2)
	case STAR(_) => true
	
	case RANGE(_) => false
	case PLUS(reg) => nullable(reg)
	case OPT(reg) => true
	case NTIMES(reg, n) => {
		if (n == 0) true
		else if (n < 0) false
		else nullable(reg)
	}

	case CHARSEQ(cl) => false
	case ANY => false
	case BOUND(reg, min, max) => {
		if (min == 0 && max >= min) true
		else if (min < 0 && max >= 0) true
		else false
	}
	case NOT(_) => false
	case NOTSEQ(cl) => false

	case MARK(_) => true
	
	case RECD(_, r1) => nullable(r1)
}

def der(c: Char, r: Rexp) : Rexp = r match {

	case ZERO => ZERO
	case ONE => ZERO
	case CHAR(d) => if (c == d) ONE else ZERO
	case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
	case SEQ(r1, r2) => 
		if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
		else SEQ(der(c, r1), r2)
	case STAR(rx) => SEQ(der(c, rx), STAR(rx))
	
	case RANGE(charSet) => if (charSet.contains(c)) ONE else ZERO
	case PLUS(reg) => SEQ(der(c, reg), STAR(reg))
	case OPT(reg) => der(c, reg)
	case NTIMES(reg, n) => {
		if (n <= 0) ZERO
		else if (n == 1) der(c, reg)
		else SEQ(der(c, reg), NTIMES(reg, n - 1))
	}

	case CHARSEQ(cl) => cl match {
		case Nil => ZERO
		case e :: Nil => der(c, CHAR(e))
		case e :: es => if (e == c) CHARSEQ(es) else ZERO
	}
	case NOTSEQ(cl) => cl match {
		case Nil => ZERO
		case e :: Nil =>
			if (e == c) ZERO
			else ONE
		case e :: es => if (e != c) NOTSEQ(es) else ZERO
	}
	case ANY => ONE
	case BOUND(r, min, max) => {
		if (min > 0 && max >= min)
			SEQ(der(c, r), BOUND(r, min - 1, max - 1))
		else if (min == 0 && max > min)
			SEQ(der(c, r), BOUND(r, min, max - 1))
		else if (min < 0 && max > 0)
			SEQ(der(c, r), BOUND(r, 0, max - 1))
		else ZERO
	}
	case NOT(charSet) => if (charSet.contains(c)) ZERO else ONE

	case MARK(_) => ZERO

	case RECD(_, r1) => der(c, r1)
}

// extracts a string from a value
def flatten(v: Val) : String = v match {

	case Empty => ""
	case Chr(c) => c.toString
	case Left(vx) => flatten(vx)
	case Right(vx) => flatten(vx)
	case Sequ(v1, v2) => flatten(v1) ++ flatten(v2)
	case Stars(vs) => vs.map(flatten).mkString 
	
	case Ranged(c) => c.toString
	case More(vx, stars) => flatten(vx) ++ flatten(stars)
	case Opted(vx) => flatten(vx)
	case Exact(vs, n) => vs.map(flatten).mkString

	case ChrSq(cl) => cl.mkString
	case AnyChar(c) => c.toString
	case Bounded(vs, n) => vs.map(flatten).mkString
	case Nein(c) => c.toString
	case NeinSq(cl) => cl.mkString
	
	case Rec(_, v) => flatten(v)

	case _ => ""
}

// extracts an environment from a value;
// used for tokenising a string
def env(v: Val) : List[(String, String)] = v match {

	case Left(vx) => env(vx)
	case Right(vx) => env(vx)
	case Sequ(v1, v2) => env(v1) ::: env(v2)
	case Stars(vs) => vs.flatMap(env)
	
	case More(vx, stars) => env(vx) ::: env(stars)
	case Opted(vx) => env(vx)
	case Exact(vs, n) => vs.flatMap(env)
	case Bounded(vs, n) => vs.flatMap(env)

	case Rec(x, vx) => (x, flatten(vx))::env(vx)

	case _ => Nil
}

// The injection and mkeps part of the lexer
//===========================================

def mkeps(r: Rexp) : Val = r match {

	case ONE => Empty
	case ALT(r1, r2) => 
		if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
	case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
	case STAR(reg) => Stars(Nil)
	
	// case RANGE(_) => throw new Exception("The reg exp RANGE is not nullable.")
	case PLUS(reg) => More(mkeps(reg), Stars(Nil))
	case OPT(reg) => Opted(Empty)
	case NTIMES(reg, n) => {
		if (n == 0) Exact(Nil, 0)
		else if (n > 0) Exact(List.fill(n)(Empty), n)
		else throw new Exception("mkeps() error, Rexp not nullable")
	}

	// case CHARSEQ(cl) => throw new Exception("By definition, the reg exp CHARSEQ is not nullable.")

	// case ANY => throw new Exception("The reg exp ANY is not nullable.")

	case BOUND(r, min, max) => {
		if (min == 0 && max >= min) Bounded(Nil, 0)
		else if (min < 0 && max >= 0) Bounded(Nil, 0)
		else if (min > 0 && max >= min) Bounded(List.fill(min)(Empty), min)
		else throw new Exception("mkeps() error, Rexp not nullable")
	}

	// case NOT(_) => throw new Exception("The reg exp NOT(s: Set[Char]) is not nullable.")

	case MARK(_) => Empty

	case RECD(x, reg) => Rec(x, mkeps(reg))
	case _ => throw new Exception("mkeps() error, Rexp not nullable")
}

def inj(r: Rexp, c: Char, v: Val) : Val = (r, v) match {

	case (STAR(reg), Sequ(v1, Stars(vs))) => Stars(inj(reg, c, v1)::vs)
	case (SEQ(r1, r2), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
	case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
	case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1), inj(r2, c, v2))
	case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
	case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))

	case (CHARSEQ(lr), Empty) => ChrSq(List(c))
	case (CHARSEQ(lr), ChrSq(lv)) if (! lv.isEmpty) => ChrSq(c :: lv)
	case (NOTSEQ(lr), Empty) => NeinSq(List(c))
	case (NOTSEQ(lr), NeinSq(lv)) if (! lv.isEmpty) => NeinSq(c :: lv)
	
	case (CHAR(d), Empty) => Chr(c)
	case (RANGE(charSet), Empty) => Ranged(c)
	case (PLUS(reg), Sequ(v1, Stars(l))) => More(inj(reg, c, v1), Stars(l))
	case (OPT(reg), _) => Opted(inj(reg, c, v))

	case (ANY, Empty) => AnyChar(c)

	case (NTIMES(reg, n), Sequ(v1, Exact(l, m))) =>
		Exact(inj(reg, c, v1) :: l, n)
	case (NTIMES(reg, n), _) => 
		Exact(inj(reg, c, v) :: Nil, 1)

	case (BOUND(reg, min, max), Sequ(v1, Bounded(l, n))) => {
		Bounded(inj(reg, c, v1) :: l, n + 1)
	}
	case (NOT(charSet), Empty) => Nein(c)

	case (RECD(x, r1), _) => Rec(x, inj(r1, c, v))

	case _ => NotMatched
}

// lexing functions without simplification
/*

def lex(r: Rexp, s: List[Char]) : Val = s match {
	case Nil => if (nullable(r)) mkeps(r) else 
		{ throw new Exception("lexing error") } 
	case c::cs => inj(r, c, lex(der(c, r), cs))
}

def value(r: Rexp, s: String) : Val = lex(r, s.toList)

def lexing(r: Rexp, s: String) = env(lex(r, s.toList))

*/

// Rectification functions

def F_ID(v: Val): Val = v

def F_RIGHT(f: Val => Val) = {
	(v:Val) => if (v == NotMatched) v
	else Right(f(v))
}

def F_LEFT(f: Val => Val) = {
	(v:Val) => if (v == NotMatched) v
	else Left(f(v))
}

def F_ALT(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
	case Right(v) => Right(f2(v))
	case Left(v) => Left(f1(v))
	case _ => NotMatched
}
def F_SEQ(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
	case Sequ(v1, v2) => Sequ(f1(v1), f2(v2))
	case _ => NotMatched
}
def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val) = {
	(v:Val) => if (v == NotMatched) v
		else Sequ(f1(Empty), f2(v))
}
	
def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val) = {
	(v:Val) => if (v == NotMatched) v
	else Sequ(f1(v), f2(Empty))
}

def F_ERROR(v: Val): Val = throw new Exception("simplification/rectification error")

// simplification
def simp(r: Rexp): (Rexp, Val => Val) = r match {
	case ALT(r1, r2) => {
		val (r1s, f1s) = simp(r1)
		val (r2s, f2s) = simp(r2)
		(r1s, r2s) match {
			case (ZERO, _) => (r2s, F_RIGHT(f2s))
			case (_, ZERO) => (r1s, F_LEFT(f1s))
			case _ => if (r1s == r2s) (r1s, F_LEFT(f1s))
								else (ALT (r1s, r2s), F_ALT(f1s, f2s)) 
		}
	}
	case SEQ(r1, r2) => {
		val (r1s, f1s) = simp(r1)
		val (r2s, f2s) = simp(r2)
		(r1s, r2s) match {
			case (ZERO, _) => (ZERO, F_ERROR)
			case (_, ZERO) => (ZERO, F_ERROR)
			case (ONE, _) => (r2s, F_SEQ_Empty1(f1s, f2s))
			case (_, ONE) => (r1s, F_SEQ_Empty2(f1s, f2s))
			case _ => (SEQ(r1s,r2s), F_SEQ(f1s, f2s))
		}
	}
	case r => (r, F_ID)
}

// lexing functions including simplification
def lex_simp(r: Rexp, s: List[Char]) : Val = s match {
	case Nil => {
		if (nullable(r)) mkeps(r)
		else {
			println(NotMatched)
			NotMatched
		} 
	}
	case c::cs => {
		val (r_simp, f_simp) = simp(der(c, r))
		inj(r, c, f_simp(lex_simp(r_simp, cs)))
	}
}

def lex(r: Rexp, s: String) = 
	env(lex_simp(r, s.toList))

// END OF FILE Tokenizer.scala

// package jucheparse

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

// class AltParser[I : IsSeq, T](pl: List[Parser]

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

// END OF FILE Parser.scala

// package jucheparse

object RegexParser {

val LETTER = RANGE((('a' to 'z') ++ ('A' to 'Z')).toSet)

val NUMBER = RANGE('0' to '9')

val OP = RANGE(Set('_', '-', '/', '%', '=', ',', '.', ':', ';', '>', '<', '~', '!', '&', '#', '`', '@'))

val QUOTE = RANGE(Set('\"', '\''))

val BRACKET = RANGE(Set('(', ')', '{', '}', '[', ']'))

val SPECIAL = RANGE(Set('\\', '*', '+', '|', '?', '$', '^'))

val SYMBOL = (OP | BRACKET | SPECIAL)

val CHARACTER = (LETTER | NUMBER | SYMBOL)

val WHITESPACE = PLUS(RANGE(Set(' ', '\n', '\t', '\r')))

val ESCAPED = (
	(CHAR('\\') ~ ((BRACKET | SPECIAL | QUOTE) | (" " | "n" | "t" | "r")))
)

val REGEX = STAR(
	("l" $ LETTER) |
	("n" $ NUMBER) |
	("op" $ OP) |
	("br" $ BRACKET) |
	("sp" $ SPECIAL) |
	("w" $ WHITESPACE) |
	("esc" $ ESCAPED)
)

case class T_L(l: Char) extends Token
case class T_N(n: Char) extends Token
case class T_OP(op: Char) extends Token
case class T_BR(br: Char) extends Token
case class T_SP(sp: Char) extends Token
case class T_ESC(esc: Char) extends Token
case object T_ERROR extends Token


val token : PartialFunction[(String, String), Token] = {
	case ("l", s) => T_L(s.head)
	case ("n", s) => T_N(s.head)
	case ("op", s) => T_OP(s.head)
	case ("br", s) => T_BR(s.head)
	case ("sp", s) => T_SP(s.head)
	case ("esc", s) => s.tail.toList match {
		case 'n' :: cs => T_ESC('\n')
		case 't' :: cs => T_ESC('\t')
		case 'r' :: cs => T_ESC('\r')
		case c :: cs => T_ESC(c)
	}
}

def tokenize(s: String) : List[Token] = 
	lex(REGEX, s).collect(token)

// END OF LEXER

// START OF PARSER

case object CharParser extends Parser[List[Token], Char] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_L(c) :: ts => Set((c, ts))
			case T_N(c) :: ts => Set((c, ts))
			case T_OP(c) :: ts => Set((c, ts))
			case T_ESC(c) :: ts => Set((c, ts))
			case _ => Set()
		}
		else Set()
	}
}

case object CommaParser extends Parser[List[Token], Char] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_OP(c) :: ts if (c == ',') => Set((c, ts))
			case _ => Set()
		}
		else Set()
	}
}

case class SpecialOp(c: Char) extends Parser[List[Token], Char] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_SP(sp) :: ts => if (sp == c) Set((c, ts)) else Set()
			case _ => Set()
		}
		else Set()
	}
}

case class BracParser(c: Char) extends Parser[List[Token], Char] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_BR(br) :: ts => if (br == c) Set((c, ts)) else Set()
			case _ => Set()
		}
		else Set()
	}
}

case object NumParser extends Parser[List[Token], Char] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_N(c) :: ts => Set((c, ts))
			case _ => Set()
		}
		else Set()
	}
}

case object LetterParser extends Parser[List[Token], Char] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_L(c) :: ts => Set((c, ts))
			case _ => Set()
		}
		else Set()
	}
}

case object UnderscoreParser extends Parser[List[Token], Char] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_OP('_') :: ts => Set(('_', ts))
			case _ => Set()
		}
		else Set()
	}
}

lazy val CharSeqParser: Parser[List[Token], List[Char]] = {
	(CharParser ~ CharSeqParser).map{case c ~ cs => c :: cs} ||
	(CharParser).map{case c => List(c)}
}

lazy val CardiParser: Parser[List[Token], Char] = {
	SpecialOp('*') || SpecialOp('+') || SpecialOp('?')
}

lazy val UnaryBlock: Parser[List[Token], Rexp] = {
	def charToReg(c: Char) : Rexp = CHAR(c)
	def AddSign(r: Rexp, c: Char) : Rexp = c match {
		case '*' => STAR(r)
		case '+' => PLUS(r)
		case '?' => OPT(r)
		case _ => r
	}

	(CharParser ~ CardiParser).map[Rexp]{
		case c ~ x => AddSign(charToReg(c), x)
	} ||
	((CharSeqReg || BracBlock) ~ CardiParser).map[Rexp]{
		case b ~ x => AddSign(b, x)
	} ||
	(BracParser('(') ~ Reg ~ BracParser(')') ~ CardiParser).map[Rexp]{
		case _ ~ b ~ _ ~ x => AddSign(b, x)
	} ||
	(SpecialOp('$')).map[Rexp]{ case _ => ANY }
}

lazy val AltReg: Parser[List[Token], Rexp] = {
	(Cluster ~ SpecialOp('|') ~ AltReg).map[Rexp]{
		case r ~ _ ~ al => ALT(r, al)
	} ||
	(Cluster ~ SpecialOp('|') ~ Cluster).map[Rexp]{
		case c1 ~ _ ~ c2 => ALT(c1, c2)
	}
}

lazy val Cluster: Parser[List[Token], Rexp] = {
	UnaryBlock || CharSeqReg ||
	BracBlock || MinMaxBlock ||
	(BracParser('(') ~ Reg ~ BracParser(')')).map[Rexp]{
		case _ ~ r ~ _ => r
	} ||
	(CharParser).map[Rexp]{ c => CHAR(c) }
}

lazy val BracBlock: Parser[List[Token], Rexp] = {
	(BracParser('[') ~ CharSeqParser ~ BracParser(']')).map[Rexp]{ case _ ~ l ~ _ => RANGE(l.toSet) } ||
	(BracParser('[') ~ SpecialOp('^') ~ CharSeqParser ~ BracParser(']')).map[Rexp]{ case _ ~ _ ~ l ~ _ => NOT(l.toSet) }
}

lazy val MinMaxBlock: Parser[List[Token], Rexp] = {
	def makeReg(r: Rexp, min: Int, max: Int) : Rexp = {
		if (min == max && min >= 0) NTIMES(r, min)
		else if (max > min && max >= 0) BOUND(r, min, max)
		else ZERO
	}

	(CharParser ~ LimitParser).map[Rexp]{
		case c ~ limit => makeReg(CHAR(c), limit._1, limit._2)
	} ||
	((BracBlock || CharSeqReg) ~ LimitParser).map[Rexp]{
		case b ~ limit => makeReg(b, limit._1, limit._2)
	} ||
	(BracParser('(') ~ Block ~ BracParser(')') ~ LimitParser).map[Rexp]{
		case _ ~ b ~ _ ~ limit => makeReg(b, limit._1, limit._2)
	}
}

lazy val LimitParser: Parser[List[Token], (Int, Int)] = {
	(BracParser('{') ~ DigitsParser ~ BracParser('}')).map{
		case _ ~ ds ~ _ => {
			val n = try {ds.mkString.toInt} catch {case e: Exception => 0}
			(n, n)
		}
	} ||
	(BracParser('{') ~ DigitsParser ~ CommaParser ~ DigitsParser ~ BracParser('}')).map{
		case _ ~ min ~ _ ~ max ~ _ => try {
			(min.mkString.toInt, max.mkString.toInt)
		} catch { case e: Exception => (0, 0) }
	}
}

lazy val DigitsParser: Parser[List[Token], List[Char]] = {
	(NumParser ~ DigitsParser).map{
		case d ~ ds => d :: ds
	} ||
	(NumParser).map{
		case d => List[Char](d)
	}
}

lazy val CharSeqReg: Parser[List[Token], Rexp] = {
	(BracParser('(') ~ CharSeqParser ~ BracParser(')')).map[Rexp]{
		case _ ~ l ~ _ => l match {
			case Nil => ONE
			case c :: Nil => CHAR(c)
			case cs => CHARSEQ(cs)
		}
	} ||
	(BracParser('(') ~ SpecialOp('^') ~ CharSeqParser ~ BracParser(')')).map[Rexp]{
		case _ ~ _ ~ l ~ _ => l match {
			case Nil => ONE
			case cs => NOTSEQ(cs)
		}
	}
}

lazy val Reference: Parser[List[Token], Rexp] = {
	(BracParser('{') ~ SpecialOp('$') ~ IdParser ~ BracParser('}')).map[Rexp]{
		case _ ~ _ ~ cs ~ _ => MARK(cs.mkString)
	}
}

lazy val IdParser: Parser[List[Token], List[Char]] = {
	(LetterParser ~ IdParser).map{
		case l ~ ls => l :: ls
	} ||
	(LetterParser ~ UnderscoreParser ~ IdParser).map{
		case l ~ u ~ ls => l :: u :: ls
	} ||
	(LetterParser || UnderscoreParser).map{
		c => List(c)
	}
}

lazy val Block: Parser[List[Token], Rexp] = {
	UnaryBlock || CharSeqReg ||
	BracBlock || MinMaxBlock ||
	AltReg || Reference
}

lazy val Reg: Parser[List[Token], Rexp] = {
	(Block).map[Rexp]{ b => b } ||
	(Block ~ Reg).map[Rexp]{
		case b ~ bs => SEQ(b, bs)
	} ||
	(BracParser('(') ~ Block ~ BracParser(')')).map[Rexp]{
		case _ ~ b ~ _ => b
	}
}

def parse(s: String) : Rexp = {
	try { Reg.parse_all(tokenize(s)).head }
	catch { case e: Exception => ZERO }
}

def parse_tokens(tl: List[Token]) : Rexp = {
	try { Reg.parse_all(tl).head }
	catch { case e: Exception => ZERO }
}

// END OF OBJECT RegexParser

}

// END OF FILE RegexParser.scala
