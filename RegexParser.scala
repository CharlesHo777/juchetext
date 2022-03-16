
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp
case class RANGE(s: Set[Char]) extends Rexp
case class PLUS(r: Rexp) extends Rexp
case class OPT(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp
case class CHARSEQ(cl: List[Char]) extends Rexp
case class ALTL(rl: List[Rexp]) extends Rexp
case object ANY extends Rexp
case class RECD(x: String, r: Rexp) extends Rexp



val LETTER = RANGE((('a' to 'z') ++ ('A' to 'Z')).toSet)

val NUMBER = RANGE('0' to '9')

val OP = RANGE(Set('\\', '_', '-', '/', '%', '=', ',', '.', ':', ';', '>', '<', '~', '^', '?', '!', '&', '`', '@'))

val BRACKET = RANGE(Set('(', ')', '{', '}', '[', ']'))

val SPECIAL = RANGE(Set('\"', '\'', '*', '+', '|', '$', '#'))

val SYMBOL = ALTL(List(OP, BRACKET, SPECIAL))

val CHARACTER = ALTL(List(LETTER, NUMBER, SYMBOL))

val WHITESPACE = PLUS(RANGE(Set(' ', '\n', '\t', '\r')))

val ESCAPED = (
	(CHAR('\\') ~ (BRACKET | SPECIAL)) |
	ALTL(List[Rexp]("\\\\", "\\ ", "\\n", "\\t", "\\r"))
)

val REGEX = STAR(
	ALTL(List[Rexp](
		("l" $ LETTER),
		("n" $ NUMBER),
		("op" $ OP),
		("br" $ BRACKET),
		("col" $ COLON),
		("q" $ QUOTE),
		("w" $ WHITESPACE),
		("esc" $ ESCAPED)
	))
)

case class T_L(l: Char) extends Token
case class T_N(n: Char) extends Token
case class T_OP(op: Char) extends Token
case class T_BR(br: Char) extends Token
case class T_COL(col: Char) extends Token
case class T_Q(q: Char) extends Token
case class T_ESC(esc: Char) extends Token
case object T_ERROR extends Token


val token : PartialFunction[(String, String), Token] = {
	try {
		case ("l", s) => T_L(s.head)
		case ("n", s) => T_N(s.head)
		case ("op", s) => T_OP(s.head)
		case ("br", s) => T_BR(s.head)
		case ("col", s) => T_COL(s.head)
		case ("q", s) => T_Q(s.head)
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










