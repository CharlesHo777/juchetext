

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
case class T_SP(col: Char) extends Token
case class T_ESC(esc: Char) extends Token
case object T_ERROR extends Token


val token : PartialFunction[(String, String), Token] = {
	try {
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
	} catch {
		case e: Exception => T_ERROR
	}
}



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

case class SpecialOp(c: Char) extends Parser[List[Token], Char] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_SP(sp) :: ts => if (sp == c) Set((c, ts)) else Set()
			case _ => Set()
		}
	}
}

case class BracParser(c: Char) extends Parser[List[Token], Char] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_BRAC(br) :: ts => if (br == c) Set((c, ts)) else Set()
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
	(BracParser('(') ~ Block ~ BracParser(')') ~ CardiParser).map[Rexp]{
		case _ ~ b ~ _ ~ x => AddSign(b, x)
	} ||
	(BracBlock ~ CardiParser).map[Rexp]{
		case b ~ x => AddSign(b, x)
	}
	(SpecialOp('$')).map[Rexp]{ case _ => ANY }
}

lazy val AltReg: Parser[List[Token], Rexp] = {
	(BracParser('(') ~ Block ~ BracParser(')') ~ SpecialOp('|') ~ AltParser).map[Rexp]{case _ ~ r ~ _ ~ _ ~ al => ALT(r1, al)} ||
	((UnaryBlock || BracBlock || MinMaxBlock) ~ SpecialOp('|') ~ AltParser).map[Rexp]{ case r ~ _ ~ al => ALT(r1, al) } ||
	(BracParser('(') ~ Block ~ BracParser(')')).map[Rexp]{r => r} ||
	(UnaryBlock || BracBlock || MinMaxBlock).map[Rexp]{r => r}
}

lazy val SeqReg: Parser[List[Token], Rexp] = {
	(BracParser('(') ~ Block ~ BracParser(')')).map[Rexp]{
		case _ ~ r ~ _ => r
	}
}

lazy val BinaryBlock: Parser[List[Token], Rexp] = {
	AltReg || SeqReg
}

lazy val BracBlock: Parser[List[Token], Rexp] = {
	(BracParser('[') ~ CharSeqParser ~ BracParser(']')).map[Rexp]{ case _ ~ l ~ _ => RANGE(l.toSet) } ||
	(BracParser('[') ~ SpecialOp('^') ~ CharSeqParser ~ BracParser(']')).map[Rexp]{ case _ ~ _ ~ l ~ _ => NOT(l.toSet) }
}

lazy val MinMaxBlock: Parser[List[Token], Rexp] = {

}

lazy val Block: Parser[List[Token], Rexp] = {
	UnaryBlock || BinaryBlock ||
	(CharSeqParser).map[Rexp]{ l match {
		case Nil => ONE
		case c :: Nil => CHAR(c)
		case cs => CHARSEQ(cs)
	}
	} ||
	BracBlock || MinMaxBlock
	}
}

lazy val RegParser: Parser[List[Token], Rexp] = {
	(Block ~ RegParser).map[Rexp]{case r ~ rs => SEQ(r, rs)} ||
	(Block).map[Rexp]{r => r}
}

case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp
case class RANGE(s: Set[Char]) extends Rexp
case class PLUS(r: Rexp) extends Rexp
case class OPT(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp
case class BOUND(r: Rexp, min: Int, max: Int) extends Rexp
case class CHARSEQ(cl: List[Char]) extends Rexp
case object ANY extends Rexp
case class NOT(c: Char) extends Rexp



