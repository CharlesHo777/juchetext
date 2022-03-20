

package jucheparse


object RegexParser {


val LETTER = RANGE((('a' to 'z') ++ ('A' to 'Z')).toSet)

val NUMBER = RANGE('0' to '9')

val OP = RANGE(Set('\"', '\'', '_', '-', '/', '%', '=', ',', '.', ':', ';', '>', '<', '~', '!', '&', '#', '`', '@'))

val BRACKET = RANGE(Set('(', ')', '{', '}', '[', ']'))

val SPECIAL = RANGE(Set('\\', '*', '+', '|', '?', '$', '^'))

val SYMBOL = (OP | BRACKET | SPECIAL)

val CHARACTER = (LETTER | NUMBER | SYMBOL)

val WHITESPACE = PLUS(RANGE(Set(' ', '\n', '\t', '\r')))

val ESCAPED = (
	(CHAR('\\') ~ ((BRACKET | SPECIAL) | (" " | "n" | "t" | "r")))
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
	/*
	def parse(in: I) = {
		val p_res = p.parse(in)
		if (p_res.isEmpty)
			q.parse(in)
		else p_res
	}*/
}

// map parser
class MapParser[I : IsSeq, T, S](p: => Parser[I, T], 
                                 f: T => S) extends Parser[I, S] {
  def parse(in: I) = for ((hd, tl) <- p.parse(in)) yield (f(hd), tl)
}

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
	}
}

lazy val Block: Parser[List[Token], Rexp] = {
	UnaryBlock || CharSeqReg ||
	BracBlock || MinMaxBlock ||
	AltReg
}

lazy val Reg: Parser[List[Token], Rexp] = {
	(Block).map[Rexp]{ b => b } ||
	(Block ~ Reg).map[Rexp]{ case b ~ bs => SEQ(b, bs) } ||
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


}



