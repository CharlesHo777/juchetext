//package dprk



// package jucheparse

// regular expressions including records
abstract class Rexp

case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp {
	override def toString : String = {
		s"CHAR(\'${c}\')"
	}
}
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp

case class RANGE(s: Set[Char]) extends Rexp {
	override def toString : String = {
		val s_mod = s.map(c => s"\'${c}\'")
		s"RANGE(Set(${s_mod.mkString("", ",", "")}))"
	}
}
case class PLUS(r: Rexp) extends Rexp
case class OPT(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp

case class CHARSEQ(cl: List[Char]) extends Rexp {
	override def toString : String = {
		val cl_mod = cl.map(c => s"\'${c}\'")
		s"CHARSEQ(${cl_mod})"
	}
}
case object ANY extends Rexp
case class BOUND(r: Rexp, min: Int, max: Int) extends Rexp
case class NOT(s: Set[Char]) extends Rexp {
	override def toString : String = {
		val s_mod = s.map(c => s"\'${c}\'")
		s"NOT(Set(${s_mod.mkString("", ",", "")}))"
	}
}
case class NOTSEQ(cl: List[Char]) extends Rexp {
	override def toString : String = {
		val cl_mod = cl.map(c => s"\'${c}\'")
		s"NOTSEQ(${cl_mod})"
	}
}

// records for extracting strings or tokens
case class RECD(x: String, r: Rexp) extends Rexp {
	override def toString : String = {
		val x_mod = s"\"${x}\""
		s"RECD(${x_mod}, ${r})"
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



object dprkTokenizer {

val LETTER = RANGE((('a' to 'z') ++ ('A' to 'Z')).toSet)

val NUMBER = RANGE('0' to '9')

val WHITESPACE = PLUS(" " | "\n" | "\t" | "\r")

val SYMBOL = (
	LETTER | NUMBER |
	RANGE(Set('+', '-', '*', '/', '%', '=', '>', '<', '.', '_', ',', ';', ':', '!', '?', '|', '&', '~','$', '#', '^', '`', '@', '(', ')', '{', '}', '[', ']', ' ')) |
	(CHAR('\\') ~ RANGE(Set('\\', '\"', '\'', 'n', 't', 'r')))
)

val INT = (OPT(CHAR('-')) ~ (CHAR('0') | (RANGE('1' to '9') ~ NUMBER.%)))
val DOUBLE = (INT ~ CHAR('.') ~ (PLUS(NUMBER)))


val Regger = SEQ(CHAR('a'),SEQ(STAR(CHAR('a')),SEQ(STAR(CHAR('b')),SEQ(ALT(CHAR('a'),ALT(CHAR('b'),CHAR('c'))),ALT(BOUND(RANGE(Set('b','a','c','d','e')),1,2),CHARSEQ(List('a', 'b', 'b', 'c', 'c', 'd')))))))


val LANGUAGE_REG = {
	STAR(
		("key" $ "good morning" | "good day" | "good afternoon" | "good evening" | "good night" | "bonsoir" | "go back to sleep you little retard" | "nighty night!" | "say" | "out" | "go" | "forward" | "meters" | "backward" | "left" | "right") |
		("sym" $ RANGE(Set[Char]())) | 
("int" $ INT)|
("db" $ DOUBLE)|
 ("Regger" $ Regger)|
		("ws" $ WHITESPACE) 
	)
}


case class T_KEY(s: String) extends Token
case class T_SYM(c: Char) extends Token
case class T_ID(s: String) extends Token
case class T_INT(n: Int) extends Token
case class T_DB(d: Double) extends Token
case class T_STR(s: String) extends Token
case class T_CHAR(c: Char) extends Token

case class T_Regger(s: String) extends Token

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
	case ("sym", s) =>
		try {T_SYM(s.head)}
		catch {case e: Exception => T_SYM('?')}
	case ("id", s) => T_ID(s)
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
	case ("char", s) =>
		try {
			val s2 = s.init.tail
			val c = process_string(s2.toList).head
			T_CHAR(c)
		} catch {
			case e: Exception => T_CHAR('?')
		}
	case ("Regger", s) => T_Regger(s)
}

def tokenize(s: String) : List[Token] = {
	lex(LANGUAGE_REG, s).collect(token)
}

}


//package dprk

object dprkParser {



// package jucheparse

case class ~[+A, +B](x: A, y: B)

// constraint for the input
type IsSeq[A] = A => Seq[_]

abstract class Parser[I : IsSeq, T]{
	
	def parse(in: I): Set[(T, I)]

	def parse_all(in: I) : Set[T] = {
		for ((hd, tl) <- parse(in); if tl.isEmpty) yield hd
	}
}

// parser combinators

// sequence parser
class SeqParser[I : IsSeq, T, S]
(p: => Parser[I, T], q: => Parser[I, S]) extends Parser[I, ~[T, S]] {
	def parse(in: I) = {
		for ((hd1, tl1) <- p.parse(in); 
				 (hd2, tl2) <- q.parse(tl1))
		yield (new ~(hd1, hd2), tl2)
	}
}

// alternative parser
class AltParser[I : IsSeq, T]
(p: => Parser[I, T], q: => Parser[I, T]) extends Parser[I, T] {
	def parse(in: I) = p.parse(in) ++ q.parse(in) 
}

// class AltParser[I : IsSeq, T](pl: List[Parser]

// map parser
class MapParser[I : IsSeq, T, S]
(p: => Parser[I, T], f: T => S) extends Parser[I, S] {
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

case class TKP(t: Token) extends Parser[List[Token], Token] {
	def parse(in: List[Token]) = {
		if (in == Nil) Set()
		else if (in.head == t) Set((t, in.tail))
		else Set()
	}
}

// END OF FILE Parser.scala



case class KWP(k: String) extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case dprkTokenizer.T_KEY(s) :: ts if (k == s) => Set((KeyNode(k), ts))
			case dprkTokenizer.T_SYM(s) :: ts if (k == s.toString) => Set((KeyNode(k), ts))
			case _ => Set()
		}
		else Set()
	}
}

case class ElemParser(k: String) extends Parser[List[Token], String] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case dprkTokenizer.T_KEY(s) :: ts if (k == s) => Set((k, ts))
			case dprkTokenizer.T_SYM(s) :: ts if (k == s.toString) => Set((k, ts))
			case _ => Set()
		}
		else Set()
	}
}

case object IdParser extends Parser[List[Token], Node] {
  def parse(tl: List[Token]) = 
  	if (tl != Nil) tl match {
  		case dprkTokenizer.T_ID(s) :: ts => Set((IdNode(s), ts))
  		case _ => Set()
  	}
  	else Set()
}

case object IntParser extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case dprkTokenizer.T_INT(n) :: ts => Set((IntNode(n), ts))
			case _ => Set()
		}
		else Set()
	}
}

case object DoubleParser extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case dprkTokenizer.T_DB(db) :: ts => Set((DoubleNode(db), ts))
			case _ => Set()
		}
		else Set()
	}
}

case object StrParser extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case dprkTokenizer.T_STR(s) :: ts => Set((StringNode(s), ts))
			case _ => Set()
		}
		else Set()
	}
}

case object CharParser extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case dprkTokenizer.T_CHAR(c) :: ts => Set((CharNode(c), ts))
			case _ => Set()
		}
		else Set()
	}
}

case class CardiParser(p: Parser[List[Token], Node], c: Cardi) extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = c match {
		case C_OPT => {
			(NothingParser || p).map{
				case n => OptionNode(n)
			}.parse(tl).asInstanceOf[Set[(Node, List[Token])]]
		}
		case C_PLUS => {
			(p ~ CardiParser(p, C_STAR)).map[Node]{
				case n1 ~ n2 => {
					if (n2.isInstanceOf[CardiNode])
						CardiNode(n1 :: n2.asInstanceOf[CardiNode].ns)
					else
						CardiNode(List[Node](n1))
				}
			}.parse(tl).asInstanceOf[Set[(Node, List[Token])]]
		}
		case C_STAR => {
			(NothingParser || (p ~ CardiParser(p, C_STAR)).map[Node]{
				case n1 ~ n2 => {
					if (n2.isInstanceOf[CardiNode])
						CardiNode(n1 :: n2.asInstanceOf[CardiNode].ns)
					else
						CardiNode(List[Node](n1))
				}
			}).parse(tl).asInstanceOf[Set[(Node, List[Token])]]
		}
	}
}

case object NothingParser extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		Set((EmptyNode, tl))
	}
}


case object ReggerParser extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case dprkTokenizer.T_Regger(s) :: ts => Set((TerminalNode(s), ts))
			case _ => Set()
		}
		else Set()
	}
}
		

abstract class Node

case class SeqNode(ns: List[Node]) extends Node
case class KeyNode(k: String) extends Node
case class OptionNode(n: Node) extends Node
case class CardiNode(ns: List[Node]) extends Node
case object EmptyNode extends Node
case class TerminalNode(s: String) extends Node

case class IdNode(s: String) extends Node
case class IntNode(n: Int) extends Node
case class DoubleNode(d: Double) extends Node
case class StringNode(s: String) extends Node
case class CharNode(c: Char) extends Node

abstract class Cardi // cardinality of an expression

case object C_OPT extends Cardi // optional
case object C_PLUS extends Cardi // one or more times
case object C_STAR extends Cardi // zero or more times

case class RobotsNode(ns: List[Node]) extends Node
case class MessageNode(ns: List[Node]) extends Node
case class GreetingNode(k: String) extends Node
case class RitualNode(ns: List[Node]) extends Node
case class MoveNode(ns: List[Node]) extends Node
case class ForwardNode(ns: List[Node]) extends Node
case class BackwardNode(ns: List[Node]) extends Node
case class LeftNode(ns: List[Node]) extends Node
case class RightNode(ns: List[Node]) extends Node



lazy val RobotsParser: Parser[List[Token], Node] = {
	(CardiParser(MessageParser, C_STAR) ~ CardiParser(MoveParser, C_STAR)).map{
		case n1 ~ n2 => RobotsNode(List(n1,n2))
	}
}

lazy val MessageParser: Parser[List[Token], Node] = {
	(GreetingParser || MoveParser || RitualParser).map{
		case n1 => MessageNode(List(n1))
	}
}

lazy val GreetingParser: Parser[List[Token], Node] = {
	(ElemParser("good morning") || ElemParser("good day") || ElemParser("good afternoon") || ElemParser("good evening") || ElemParser("good night") || ElemParser("bonsoir") || ElemParser("go back to sleep you little retard") || ElemParser("nighty night!")).map[Node]{
		case s => GreetingNode(s)
	}
}
		
lazy val RitualParser: Parser[List[Token], Node] = {
	(KWP("say") ~ KWP("out") ~ GreetingParser ~ KWP("go") ~ KWP("forward") ~ IntParser ~ KWP("meters")).map{
		case _ ~ _ ~ n1 ~ _ ~ _ ~ n2 ~ _ => RitualNode(List(n1,n2))
	}
}

lazy val MoveParser: Parser[List[Token], Node] = {
	(ForwardParser || BackwardParser || LeftParser || RightParser).map{
		case n1 => MoveNode(List(n1))
	}
}

lazy val ForwardParser: Parser[List[Token], Node] = {
	(KWP("go") ~ KWP("forward") ~ IntParser ~ KWP("meters")).map{
		case _ ~ _ ~ n1 ~ _ => ForwardNode(List(n1))
	}
}

lazy val BackwardParser: Parser[List[Token], Node] = {
	(KWP("go") ~ KWP("backward") ~ IntParser ~ KWP("meters")).map{
		case _ ~ _ ~ n1 ~ _ => BackwardNode(List(n1))
	}
}

lazy val LeftParser: Parser[List[Token], Node] = {
	(KWP("go") ~ KWP("left") ~ DoubleParser ~ KWP("meters")).map{
		case _ ~ _ ~ n1 ~ _ => LeftNode(List(n1))
	}
}

lazy val RightParser: Parser[List[Token], Node] = {
	(KWP("go") ~ KWP("right") ~ DoubleParser ~ KWP("meters")).map{
		case _ ~ _ ~ n1 ~ _ => RightNode(List(n1))
	}
}


def parse(s: String) : Node = {
	val tks = dprkTokenizer.tokenize(s)
	try {
		RobotsParser.parse_all(tks).head
	} catch {
		case e: Exception => RobotsNode(Nil)
	}
}

}
