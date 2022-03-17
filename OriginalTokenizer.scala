

// regular expressions including records
abstract class Rexp

case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp

case class RANGE(s: Set[Char]) extends Rexp
case class PLUS(r: Rexp) extends Rexp
case class OPT(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp

// records for extracting strings or tokens
case class RECD(x: String, r: Rexp) extends Rexp

/* def RANGE takes a parameter of type collection.immutable.NumericRange[Char],
the output value is of type Rexp and is equivalent to case class RANGE */
def RANGE(range: collection.immutable.NumericRange[Char]): Rexp = {
	RANGE(range.toSet)
}

// values  
abstract class Val

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


abstract class Token

// some convenience for typing in regular expressions

def charlist2rexp(s : List[Char]): Rexp = s match {
	case Nil => ONE
	case c::Nil => CHAR(c)
	case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

implicit def string2rexp(s : String) : Rexp = 
	charlist2rexp(s.toList)

implicit def RexpOps(r: Rexp) = new {
	def | (s: Rexp) = ALT(r, s)
	def % = STAR(r)
	def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps(s: String) = new {
	def | (r: Rexp) = ALT(s, r)
	def | (r: String) = ALT(s, r)
	def % = STAR(s)
	def ~ (r: Rexp) = SEQ(s, r)
	def ~ (r: String) = SEQ(s, r)
	def $ (r: Rexp) = RECD(s, r)
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
	case NTIMES(reg, n) => if (n == 0) true else nullable(reg)
	
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
	case STAR(r) => SEQ(der(c, r), STAR(r))
	
	case RANGE(charSet) => if (charSet.contains(c)) ONE else ZERO
	case PLUS(reg) => SEQ(der(c, reg), STAR(reg))
	case OPT(reg) => der(c, reg)
	case NTIMES(reg, n) =>
		if (n <= 0) ZERO
		else if (n == 1) der(c, reg)
		else SEQ(der(c, reg), NTIMES(reg, n - 1))
		
	case RECD(_, r1) => der(c, r1)
}

// extracts a string from a value
def flatten(v: Val) : String = v match {

	case Empty => ""
	case Chr(c) => c.toString
	case Left(v) => flatten(v)
	case Right(v) => flatten(v)
	case Sequ(v1, v2) => flatten(v1) ++ flatten(v2)
	case Stars(vs) => vs.map(flatten).mkString 
	
	case Ranged(c) => c.toString
	case More(v, stars) => flatten(v) ++ flatten(stars)
	case Opted(v) => flatten(v)
	case Exact(vs, n) => vs.map(flatten).mkString
	
	case Rec(_, v) => flatten(v)
}

// extracts an environment from a value;
// used for tokenising a string
def env(v: Val) : List[(String, String)] = v match {

	case Empty => Nil
	case Chr(c) => Nil
	case Left(v) => env(v)
	case Right(v) => env(v)
	case Sequ(v1, v2) => env(v1) ::: env(v2)
	case Stars(vs) => vs.flatMap(env)
	
	case Ranged(c) => Nil
	case More(v, stars) => env(v) ::: env(stars)
	case Opted(v) => env(v)
	case Exact(vs, n) => vs.flatMap(env)
	
	case Rec(x, v) => (x, flatten(v))::env(v)
}

// The injection and mkeps part of the lexer
//===========================================

def mkeps(r: Rexp) : Val = r match {

	case ONE => Empty
	case ALT(r1, r2) => 
		if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
	case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
	case STAR(reg) => Stars(Nil)
	
	case RANGE(_) => throw new Exception("RANGE is not nullable.")
	case PLUS(reg) => More(mkeps(reg), Stars(Nil))
	case OPT(reg) => Opted(Empty)
	case NTIMES(reg, n) =>
		if (n == 0) Exact(Nil, 0)
		else if (n > 0) Exact(List.fill(n)(Empty), n)
		else throw new Exception("In NTIMES(r, n), n cannot be smaller than 0.")
		
	case RECD(x, reg) => Rec(x, mkeps(reg))
}

def inj(r: Rexp, c: Char, v: Val) : Val = (r, v) match {

	case (STAR(reg), Sequ(v1, Stars(vs))) => Stars(inj(reg, c, v1)::vs)
	case (SEQ(r1, r2), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
	case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
	case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1), inj(r2, c, v2))
	case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
	case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
	case (CHAR(d), Empty) => Chr(c) 
	
	case (RANGE(charList), _) => Ranged(c)
	case (PLUS(reg), Sequ(v1, Stars(l))) => More(inj(reg, c, v1), Stars(l))
	case (OPT(reg), _) => Opted(inj(reg, c, v))
	case (NTIMES(reg, n), Sequ(v1, Exact(l, m))) =>
		if (m == n - 1) Exact(inj(reg, c, v1) :: l, m + 1)
		else throw new Exception("The injection process involving NTIMES and Exact is faulty.")
	case (NTIMES(reg, n), _) => Exact(inj(reg, c, v) :: Nil, 1)
	
	case (RECD(x, r1), _) => Rec(x, inj(r1, c, v))
}

// lexing functions without simplification
def lex(r: Rexp, s: List[Char]) : Val = s match {
	case Nil => if (nullable(r)) mkeps(r) else 
		{ throw new Exception("lexing error") } 
	case c::cs => inj(r, c, lex(der(c, r), cs))
}

def value(r: Rexp, s: String) : Val = lex(r, s.toList)

def lexing(r: Rexp, s: String) = 
	env(lex(r, s.toList))

// Rectification functions

def F_ID(v: Val): Val = v

def F_RIGHT(f: Val => Val) = (v:Val) => Right(f(v))

def F_LEFT(f: Val => Val) = (v:Val) => Left(f(v))

def F_ALT(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
	case Right(v) => Right(f2(v))
	case Left(v) => Left(f1(v))
}
def F_SEQ(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
	case Sequ(v1, v2) => Sequ(f1(v1), f2(v2))
}
def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val) = 
	(v:Val) => Sequ(f1(Empty), f2(v))
	
def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val) = 
	(v:Val) => Sequ(f1(v), f2(Empty))

def F_ERROR(v: Val): Val = throw new Exception("error")

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
	case Nil => if (nullable(r)) mkeps(r) else 
		{ throw new Exception("lexing error") } 
	case c::cs => {
		val (r_simp, f_simp) = simp(der(c, r))
		inj(r, c, f_simp(lex_simp(r_simp, cs)))
	}
}

def lex(r: Rexp, s: String) = 
	env(lex_simp(r, s.toList))




val KEYS = ("rule" | "enumerate" | "terminal" | "returns" | "current" | "hidden" | "abstract" | "component" | "ID" | "INT" | "DOUBLE" | "STRING" | "grammar" | "with" | "generate" | "program" | "import" | "as")


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

val CHARACTER = (CHAR('\'') ~ (SYMBOL | RANGE(Set(' ', '\n', '\t', '\r')) | ("\\\\" | "\\\"" | "\\\'" | "\\n" | "\\t" | "\\r")) ~ CHAR('\''))


val GRAMMAR_LANG = {
	STAR(
		("key" $ KEYS) |
		("id" $ IDENTIFIER) |
		("op" $ OP) |
		("int" $ INT) |
		("db" $ DOUBLE) |
		("str" $ STRING) |
		("char" $ CHARACTER) |
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



