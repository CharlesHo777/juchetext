
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

// START OF FILE Parser.scala

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

// START OF FILE LexerTemplate.scala

// package jucheparse

object LexerTemplate {

val code = """

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

"""

def get : String = code

}

// END OF FILE LexerTemplate.scala

// package jucheparse

object ParserTemplate {

val code = """

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

"""

def get : String = code

}

// package jucheparse

object RegexParser {

val LETTER = RANGE((('a' to 'z') ++ ('A' to 'Z')).toSet)

val NUMBER = RANGE('0' to '9')

val OP = RANGE(Set('_', '/', '%', '=', ',', '.', ':', ';', '>', '<', '~', '!', '&', '#', '`', '@'))

val QUOTE = RANGE(Set('\"', '\''))

val BRACKET = RANGE(Set('(', ')', '{', '}', '[', ']'))

val SPECIAL = RANGE(Set('\\', '*', '+', '-', '|', '?', '$', '^'))

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
	(BracParser('[') ~ BracContent ~ BracParser(']')).map[Rexp]{ case _ ~ s ~ _ => RANGE(s) } ||
	(BracParser('[') ~ SpecialOp('^') ~ BracContent ~ BracParser(']')).map[Rexp]{ case _ ~ _ ~ s ~ _ => NOT(s) }
}

lazy val BracContent: Parser[List[Token], Set[Char]] = {
	((CharSeqParser || RangeParser) ~ BracContent).map{
		case l ~ s => l.toSet ++ s
	} ||
	(CharSeqParser || RangeParser).map{
		case l => l.toSet
	}
}

lazy val RangeParser: Parser[List[Token], List[Char]] = {
	(BracParser('(') ~ CharParser ~ SpecialOp('-') ~ CharParser ~ BracParser(')')).map{
		case _ ~ c1 ~ _ ~ c2 ~ _ => (c1 to c2).toList
	}
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

// START OF FILE Grammar.scala

// package jucheparse

/*
	This .scala file contains the parser for the grammar file containing the syntax definitions of a user-defined programming language. The output of this parser would be an abstract syntax tree that would be further processed with the ultimate goal of generating a lexical analyzer and a parser for the aforementioned programming language.
*/

object Grammar {

// Lexer & Parser For The Grammar Language

val KEYS = ("rule" | "enumerate" | "terminal" | "fragment" | "ID" | "INT" | "DOUBLE" | "STRING" | "CHAR" | "grammar" | "program" | "ignore" | "min" | "max")

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

case class Assign(e: Exp) extends Exp
case class Keyword(s: String) extends Exp
case class CallRule(r: String) extends Exp
case class AltExp(e1: Exp, e2: Exp) extends Exp
case class TypeExp(t: String) extends Exp
case class CardiExp(e: Exp, c: Cardi) extends Exp
case class SeqExp(es: List[Exp]) extends Exp

case class Elem(s: String) extends Exp

abstract class Cardi
case object C_OPT extends Cardi
case object C_PLUS extends Cardi
case object C_STAR extends Cardi
//case class C_EXACT(n: Int) extends Cardi
//case class C_MIN(min: Int) extends Cardi
//case class C_MINMAX(min: Int, max: Int) extends Cardi

case object CardiParser extends Parser[List[Token], Cardi] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_OP('*') :: ts => Set((C_STAR, ts))
			case T_OP('+') :: ts => Set((C_PLUS, ts))
			case T_OP('?') :: ts => Set((C_OPT, ts))
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
	(OpParser('$') ~ Exp).map[Exp]{
		case _ ~ e => Assign(e)
	} ||
	(StrParser).map[Exp]{
		case s => Keyword(s)
	} ||
  (IdParser).map[Exp]{
		case r => CallRule(r)
	} ||
  (BracParser('(') ~ Block ~ BracParser(')') ~ CardiParser).map[Exp]{
		case _ ~ es ~ _ ~ c =>
			if (es.size == 1) CardiExp(es.head, c)
			else CardiExp(SeqExp(es), c)
	} ||
	(TypeParser).map[Exp]{
		case t => TypeExp(t)
	} ||
  (BracParser('(') ~ Block ~ BracParser(')')).map[Exp]{
		case _ ~ es ~ _ =>
			if (es.size == 1) es.head
			else SeqExp(es)
	}
}

/*
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
*/

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
	(ElemParser ~ OpParser('|') ~ Enum).map{
		case e ~ _ ~ en => e :: en
	} ||
	ElemParser.map{e => List(e)}
}

lazy val ElemParser: Parser[List[Token], Elem] = {
  (StrParser || IdParser).map[Elem]{s => Elem(s)}
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

// START OF FILE LexerGenerator.scala

// package jucheparse

object LexerGenerator {

	def traverse_exp(e: Grammar.Exp) : List[String] = e match {
		case Grammar.Assign(e) => traverse_exp(e)
		case Grammar.Keyword(s) => List(s)
		case Grammar.AltExp(e1, e2) => traverse_exp(e1) ::: traverse_exp(e2)
		case Grammar.CardiExp(e, _) => traverse_exp(e)
		case Grammar.SeqExp(es) => traverse_exps(es)
		case _ => Nil
	}

	def traverse_exps(el: List[Grammar.Exp]) : List[String] = el match {
		case e :: es => traverse_exp(e) ::: traverse_exps(es)
		case Nil => Nil
	}

	def traverse_elems(el: List[Grammar.Elem]) : List[String] = el match {
		case e :: es => e.s :: traverse_elems(es)
		case Nil => Nil
	}

	def traverse_stmt(s: Grammar.Stmt) : List[String] = s match {
		case Grammar.Program(_, el) => traverse_exps(el)
		case Grammar.Rule(_, el) => traverse_exps(el)
		case Grammar.Enumerate(_, el) => traverse_elems(el)
		case _ => Nil
	}

	def reg_to_rexp(reg: String) : Rexp = {
		val r = RegexParser.parse(reg)
		if (r == ZERO) ONE
		else r
	}

	def build_alt_keys(xs: List[String]) : String = xs match {
		case Nil => "ZERO"
		case k :: Nil => s""""${k}""""
		case k :: ks => s""""${k}" | ${build_alt_keys(ks)}"""
	}

	def stmts_to_terminals(ls: List[Grammar.Stmt]) : List[Grammar.Terminal] = ls match {
		case Nil => Nil
		case t :: sx if t.isInstanceOf[Grammar.Terminal] => t.asInstanceOf[Grammar.Terminal] :: stmts_to_terminals(sx)
		case s :: sx => stmts_to_terminals(sx)
	}

	def select_fragments(ls: List[Grammar.Terminal]) : List[Grammar.Terminal] = ls match {
		case Nil => Nil
		case t :: sx if (t.frag == true) =>
			t :: select_fragments(sx)
		case t :: sx => select_fragments(sx)
	}

	def select_non_fragments(ls: List[Grammar.Terminal]) : List[Grammar.Terminal] = ls match {
		case Nil => Nil
		case t :: sx if (t.frag == false) =>
			t :: select_non_fragments(sx)
		case t :: sx => select_non_fragments(sx)
	}

	def terminals_def_as_string(tl: List[Grammar.Terminal]) : String = tl match {
		case Nil => ""
		case t :: ts => {
			s"""val ${t.id} = ${t.pat}
			|""".stripMargin ++ terminals_def_as_string(ts)
		}
	}

	def find_types_in_exps(el: List[Grammar.Exp]) : List[String] = el match {
		case Nil => Nil
		case Grammar.TypeExp(t) :: es => t :: find_types_in_exps(es)
		case Grammar.Assign(e) :: es => find_types_in_exps(List[Grammar.Exp](e)) ::: find_types_in_exps(es)
		case e :: es => find_types_in_exps(es)
	}
	
	def find_types_in_stmts(ls: List[Grammar.Stmt]) : List[String] = ls match {
		case Nil => Nil
		case Grammar.Rule(_, exps) :: xs => find_types_in_exps(exps) ::: find_types_in_stmts(xs)
		case Grammar.Program(_, exps) :: xs => find_types_in_exps(exps) ::: find_types_in_stmts(xs)
		case s :: xs => find_types_in_stmts(xs)
	}

	def build_types(sl: List[String]) : String = sl match {
		case Nil => ""
		case "ID" :: xs => {
			"""val IDENTIFIER = (LETTER ~ STAR(CHAR('_') | LETTER | |NUMBER))
			|""".stripMargin ++ build_types(xs)
		}
		case "INT" :: xs => {
			"""val INT = (OPT(CHAR('-')) ~ (CHAR('0') | (RANGE('1' to '9') ~ NUMBER.%)))
			|""".stripMargin ++ build_types(xs)
		}
		case "DOUBLE" :: xs => {
			"""val DOUBLE = (INT ~ CHAR('.') ~ (PLUS(NUMBER)))
			|""".stripMargin ++ build_types(xs)
		}
		case "STRING" :: xs => {
			"""val STRING = (CHAR('\"') ~ STAR(SYMBOL | '\'') ~ CHAR('\"'))
			|""".stripMargin ++ build_types(xs)
		}
		case "CHAR" :: xs => {
			"""val CHARACTER = (CHAR('\'') ~ (SYMBOL | '\"') ~ CHAR('\''))
			|""".stripMargin ++ build_types(xs)
		}
		case _ :: xs => ""
	}

	def build_types_as_recd_rexp(sl: List[String]) : List[String] = sl match {
		case Nil => Nil
		case "ID" :: ts => s"""("id" $$ ID)""" :: build_types_as_recd_rexp(ts)
		case "INT" :: ts => s"""("int" $$ INT)""" :: build_types_as_recd_rexp(ts)
		case "DOUBLE" :: ts => s"""("db" $$ DOUBLE)""" :: build_types_as_recd_rexp(ts)
		case "STRING" :: ts => s"""("str" $$ STRING)""" :: build_types_as_recd_rexp(ts)
		case "CHAR" :: ts => s"""("char" $$ CHARACTER)""" :: build_types_as_recd_rexp(ts)
		case t :: ts => build_types_as_recd_rexp(ts)
	}

	val template = LexerTemplate.get

	def generate(source: String) : String = {

		val ls = Grammar.parse(source)

		val title = ls match {
			case t :: sx if (t.isInstanceOf[Grammar.Title]) => t.asInstanceOf[Grammar.Title].p
			case _ => "not_named"
		}

		val types = find_types_in_stmts(ls)

		val types_def = build_types(types.distinct)

		val terminals = stmts_to_terminals(ls)

		val terminals_def = terminals_def_as_string(terminals.distinct)

		val kwds = ls.flatMap(traverse_stmt).distinct

		val kwds_words = kwds.filter(k => k.size > 1)

		val kwds_chars = kwds.filter(k => k.size == 1)

		val kwds_words_reg = s"""("key" $$ ${build_alt_keys(kwds)})"""

		def print_chars(sc: List[String]) : String = sc match {
			case Nil => ""
			case s :: Nil => s"\'${s}\'"
			case s :: sx => s"\'${s}\', " ++ print_chars(sx)
		}

		val kwds_chars_reg = s"""("sym" $$ RANGE(Set[Char](${print_chars(kwds_chars)})))"""

		val types_recds = {
			val rxds = build_types_as_recd_rexp(types.distinct)
			if (! rxds.isEmpty) rxds.mkString("\n", "|\n", "|\n")
			else ""
		}

		val non_fragments = select_non_fragments(terminals).distinct

		val non_fragments_cases_list = non_fragments.map[String](t => s"""case ("${t.id}", s) => T_${t.id}(s)""")

		val terminal_recds_list = non_fragments.map(t => s"""("${t.id}" $$ ${t.id})""")

		val terminal_recds = {
			if (! terminal_recds_list.isEmpty) terminal_recds_list.mkString("", "|\n", "|")
			else ""
		}

		val terminals_tokens = non_fragments.map(t => s"""case class T_${t.id}(s: String) extends Token""").mkString("", "\n", "")

		val hiddens = ls.filter(s => s.isInstanceOf[Grammar.Hidden]).map[Grammar.Hidden](s => s.asInstanceOf[Grammar.Hidden])

		val hidden_recds_list = hiddens.map(h => s"""("${h.id}" $$ ${h.pat})""")

		val hidden_recds = {
			if (! hidden_recds_list.isEmpty) hidden_recds_list.mkString("|\n(", "|\n", ")")
			else ""
		}

		val define_lang : String = {
			s"""val LANGUAGE_REG = {
			|	STAR(
			|		${kwds_words_reg} |
			|		${kwds_chars_reg} | ${types_recds} ${terminal_recds}
			|		("ws" $$ WHITESPACE) ${hidden_recds}
			|	)
			|}"""
		}

		val terminal_cases = {
			if (! non_fragments_cases_list.isEmpty)
				non_fragments_cases_list.mkString("", "\n", "")
			else ""
		}

		val token_partial_function = s"""
		|val token : PartialFunction[(String, String), Token] = {
		|	case ("key", s) => T_KEY(s)
		|	case ("sym", s) =>
		|		try {T_SYM(s.head)}
		|		catch {case e: Exception => T_SYM('?')}
		|	case ("id", s) => T_ID(s)
		|	case ("int", s) =>
		|		try {T_INT(s.toInt)}
		|		catch {case e: Exception => T_INT(0)}
		|	case ("db", s) =>
		|		try {T_DB(s.toDouble)}
		|		catch {case e: Exception => T_DB(0.0)}
		|	case ("str", s) =>
		|		try {
		|			val s2 = s.init.tail
		|			val s3 = process_string(s2.toList).mkString
		|			T_STR(s3)
		|		} catch {
		|			case e: Exception => T_STR("")
		|		}
		|	case ("char", s) =>
		|		try {
		|			val s2 = s.init.tail
		|			val c = process_string(s2.toList).head
		|			T_CHAR(c)
		|		} catch {
		|			case e: Exception => T_CHAR('?')
		|		}
		|	${terminal_cases}
		|}"""

		// Below is the return value

		s"""
		|package ${title}
		|
		|${template}
		|
		|object ${title}Tokenizer {
		|
		|val LETTER = RANGE((('a' to 'z') ++ ('A' to 'Z')).toSet)
		|
		|val NUMBER = RANGE('0' to '9')
		|
		|val WHITESPACE = PLUS(" " | "\\n" | "\\t" | "\\r")
		|
		|val SYMBOL = (
		|	LETTER | NUMBER |
		|	RANGE(Set('+', '-', '*', '/', '%', '=', '>', '<', '.', '_', ',', ';', ':', '!', '?', '|', '&', '~','$$', '#', '^', '`', '@', '(', ')', '{', '}', '[', ']', ' ')) |
		|	(CHAR('\\\\') ~ RANGE(Set('\\\\', '\\"', '\\'', 'n', 't', 'r')))
		|)
		|
		|${types_def}
		|
		|${terminals_def}
		|
		|${define_lang}
		|
		|
		|case class T_KEY(s: String) extends Token
		|case class T_SYM(c: Char) extends Token
		|case class T_ID(s: String) extends Token
		|case class T_INT(n: Int) extends Token
		|case class T_DB(d: Double) extends Token
		|case class T_STR(s: String) extends Token
		|case class T_CHAR(c: Char) extends Token
		|
		|${terminals_tokens}
		|
		|def process_string(s: List[Char]) : List[Char] = s match {
		|case '\\\\' :: '\\\\' :: cs => '\\\\' :: process_string(cs)
		|case '\\\\' :: '\\\"' :: cs => '\\\"' :: process_string(cs)
		|case '\\\\' :: '\\\'' :: cs => '\\\'' :: process_string(cs)
		|case '\\\\' :: 'n' :: cs => '\\n' :: process_string(cs)
		|case '\\\\' :: 't' :: cs => '\\t' :: process_string(cs)
		|case '\\\\' :: 'r' :: cs => '\\r' :: process_string(cs)
		|case c :: cs => c :: process_string(cs)
		|case Nil => Nil
		|}
		|
		|${token_partial_function}
		|
		|def tokenize(s: String) : List[Token] = {
  	|	lex(LANGUAGE_REG, s).collect(token)
		|}
		|
		|}
		|
		""".stripMargin
	}

}

// END OF FILE LexerGenerator.scala

// START OF FILE ParserGenerator.scala

// package jucheparse

object ParserGenerator {

def stmts_to_terminals(ls: List[Grammar.Stmt]) : List[Grammar.Terminal] = ls match {
	case Nil => Nil
	case t :: sx if t.isInstanceOf[Grammar.Terminal] => t.asInstanceOf[Grammar.Terminal] :: stmts_to_terminals(sx)
	case s :: sx => stmts_to_terminals(sx)
}

def select_non_fragments(ls: List[Grammar.Terminal]) : List[Grammar.Terminal] = ls match {
	case Nil => Nil
	case t :: sx if (t.frag == false) =>
		t :: select_non_fragments(sx)
	case t :: sx => select_non_fragments(sx)
}

def build_terminal_parsers(ls: List[Grammar.Stmt]) : String = {
	val title = ls match {
		case t :: sx if (t.isInstanceOf[Grammar.Title]) => t.asInstanceOf[Grammar.Title].p
		case _ => "not_named"
	}

	val non_fragments = select_non_fragments(stmts_to_terminals(ls))

	val parser_list = non_fragments.map(t => t.id).map(
		t_name => s"""
		|case object ${t_name}Parser extends Parser[List[Token], Node] {
		|	def parse(tl: List[Token]) = {
		|		if (tl != Nil) tl match {
		|			case ${title}Tokenizer.T_${t_name}(s) :: ts => Set((TerminalNode(s), ts))
		|			case _ => Set()
		|		}
		|		else Set()
		|	}
		|}
		""".stripMargin
	)
	parser_list.mkString("", "", "")
}

def create_node(id: String) : String = {
	s"case class ${id}Node(ns: List[Node]) extends Node\n"
}

def create_nodes(ls: List[Grammar.Stmt]) : String = ls match {
	case Grammar.Rule(id, _) :: sx => create_node(id) ++ create_nodes(sx)
	case Grammar.Program(id, _) :: sx => create_node(id) ++ create_nodes(sx)
	case Grammar.Enumerate(id, _) :: sx => s"case class ${id}Node(k: String) extends Node\n" ++ create_nodes(sx)
	case _ :: sx => create_nodes(sx)
	case Nil => ""
}

def retrieve_exps_from_rule(r: Grammar.Rule) : List[Grammar.Exp] = {
	r.exps
}

def retrieve_exps_from_program(p: Grammar.Program) : List[Grammar.Exp] = {
	p.exps
}

def build_type_parser(t: String) : String = t match {
	case "ID" => "IdParser"
	case "INT" => "IntParser"
	case "DOUBLE" => "DoubleParser"
	case "STRING" => "StrParser"
	case "CHAR" => "CharParser"
	case _ => "NothingParser"
}

def build_cardi_parser(e: Grammar.Exp, c: Grammar.Cardi) : String = c match {
	case Grammar.C_OPT => s"CardiParser(${build_parser(e)}, C_OPT)"
	case Grammar.C_PLUS => s"CardiParser(${build_parser(e)}, C_PLUS)"
	case Grammar.C_STAR => s"CardiParser(${build_parser(e)}, C_STAR)"
	case _ => s"NothingParser"
}

def build_alt_parser(e1: Grammar.Exp, e2: Grammar.Exp) : String = {
	s"${build_parser(e1)} || ${build_parser(e2)}"
}

def build_seq_parser(el: List[Grammar.Exp]) : String = {
	if (! el.isEmpty)
		el.map(build_parser).mkString("", " ~ ", "")
	else
		"NothingParser"
}

def parse_results(el: List[Grammar.Exp], i: Int = 1) : List[String] = el match {
	case Grammar.Assign(_) :: es => s"n${i}" :: parse_results(es, i + 1)
	case _ :: es => "_" :: parse_results(es, i)
	case Nil => Nil
}

def print_parse_results(el: List[Grammar.Exp]) : (String, Int) = {
	val res = parse_results(el)
	val n = res.count(r => r != "_")
	if (! res.isEmpty)
		(res.mkString("", " ~ ", ""), n)
	else
		("_", 0)
}

def node_list(count: Int) : String = {
	val l = (1 to count).toList
	l.map(i => s"n${i}").mkString("", ",", "")
}

def build_parser(e: Grammar.Exp) : String = e match {
	case Grammar.Keyword(s) => s"KWP(\"${s}\")"
	case Grammar.CallRule(r) => s"${r}Parser"
	case Grammar.Assign(e) => build_parser(e)
	case Grammar.TypeExp(t) => build_type_parser(t)
	case Grammar.CardiExp(e, c) => build_cardi_parser(e, c)
	case Grammar.AltExp(e1, e2) => s"${build_alt_parser(e1, e2)}"
	case Grammar.SeqExp(es) => {
		val (res, n) = print_parse_results(es)
		val node = {
			if (n <= 0) "EmptyNode"
			else if (n == 1) "n1"
			else s"SeqNode(List(${node_list(n)}))"
		}
		s"(${build_seq_parser(es)}).map{case ${res} => ${node}}"
	}
	case _ => ""
}

def build_parser_sequence(el: List[Grammar.Exp]) : String = {
	val parser_list = el.map(e => s"(${build_parser(e)})")
	
	if (parser_list.size > 1)
		parser_list.mkString("(", " ~ ", ")")
	else if (parser_list.size == 1)
		parser_list.head
	else
		"NothingParser"
}

def list_elems(el: List[Grammar.Elem]) : String = {
	if (! el.isEmpty) {
		val elem_parsers = el.map(e => s"ElemParser(\"${e.s}\")")
		elem_parsers.mkString("", " || ", "")
	}
	else
		"NothingParser"
}

def build_stmt_parser(s: Grammar.Stmt) : String = {
	if (s.isInstanceOf[Grammar.Rule]) {
		val r = s.asInstanceOf[Grammar.Rule]
		val el = r.exps
		val exps_seq = {
			if (! el.isEmpty)
				el.map(build_parser).mkString("(", " ~ ", ")")
			else
				"NothingParser"
		}
		val (res, n) = print_parse_results(el)
		s"""
		|lazy val ${r.id}Parser: Parser[List[Token], Node] = {
		|	${exps_seq}.map{
		|		case ${res} => ${r.id}Node(List(${node_list(n)}))
		|	}
		|}
		|""".stripMargin
	}
	else if (s.isInstanceOf[Grammar.Program]) {
		val p = s.asInstanceOf[Grammar.Program]
		build_stmt_parser(Grammar.Rule(p.id, p.exps))
	}
	else if (s.isInstanceOf[Grammar.Enumerate]) {
		val en = s.asInstanceOf[Grammar.Enumerate]
		val el = en.el
		s"""
		|lazy val ${en.id}Parser: Parser[List[Token], Node] = {
		|	(${list_elems(el)}).map[Node]{
		|		case s => ${en.id}Node(s)
		|	}
		|}
		""".stripMargin
	}
	else ""
}

def generate(source: String) : String = {

val ls = Grammar.parse(source)

val title = ls match {
	case t :: sx if (t.isInstanceOf[Grammar.Title]) => t.asInstanceOf[Grammar.Title].p
	case _ => "not_named"
}

val template = ParserTemplate.get

val program_rule = ls.find(s => s.isInstanceOf[Grammar.Program]).getOrElse(Grammar.Program("null", Nil))

val program_name = program_rule.asInstanceOf[Grammar.Program].id

s"""

package ${title}

object ${title}Parser {

${template}

case class KWP(k: String) extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case ${title}Tokenizer.T_KEY(s) :: ts if (k == s) => Set((KeyNode(k), ts))
			case ${title}Tokenizer.T_SYM(c) :: ts if (k == c.toString) => Set((KeyNode(k), ts))
			case _ => Set()
		}
		else Set()
	}
}

case class ElemParser(k: String) extends Parser[List[Token], String] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case ${title}Tokenizer.T_KEY(s) :: ts if (k == s) => Set((k, ts))
			case ${title}Tokenizer.T_SYM(c) :: ts if (k == c.toString) => Set((k, ts))
			case _ => Set()
		}
		else Set()
	}
}

case object IdParser extends Parser[List[Token], Node] {
  def parse(tl: List[Token]) = 
  	if (tl != Nil) tl match {
  		case ${title}Tokenizer.T_ID(s) :: ts => Set((IdNode(s), ts))
  		case _ => Set()
  	}
  	else Set()
}

case object IntParser extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case ${title}Tokenizer.T_INT(n) :: ts => Set((IntNode(n), ts))
			case _ => Set()
		}
		else Set()
	}
}

case object DoubleParser extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case ${title}Tokenizer.T_DB(db) :: ts => Set((DoubleNode(db), ts))
			case _ => Set()
		}
		else Set()
	}
}

case object StrParser extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case ${title}Tokenizer.T_STR(s) :: ts => Set((StringNode(s), ts))
			case _ => Set()
		}
		else Set()
	}
}

case object CharParser extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case ${title}Tokenizer.T_CHAR(c) :: ts => Set((CharNode(c), ts))
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

${build_terminal_parsers(ls)}

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

${create_nodes(ls)}

${ls.map(build_stmt_parser).mkString("", "", "")}

def parse(s: String) : Node = {
	val tks = ${title}Tokenizer.tokenize(s)
	try {
		${program_name}Parser.parse_all(tks).head
	} catch {
		case e: Exception => ${program_name}Node(Nil)
	}
}

}

"""

}

}

// END OF FILE ParserGenerator.scala
