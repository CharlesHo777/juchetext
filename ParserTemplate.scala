
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
