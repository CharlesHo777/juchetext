package juchetext



// START OF LEXER




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

def lexing_simp(r: Rexp, s: String) = 
  env(lex_simp(r, s.toList))

// Start of Question 1

val KEYWORD = (("if" | "then" | "else") | ("def" | "Int" | "Double" | "Void") | ("val" | "true" | "false" | "skip"))
	
val AOP = RANGE(Set('+', '-', '*', '%', '/', '='))

val BOP = ((RANGE(Set('=', '!', '<', '>')) ~ CHAR('=')) | ( CHAR('>') | CHAR('<')) | ("&&" | "||"))

val LETTER = RANGE((('a' to 'z') ++ ('A' to 'Z')).toSet)

val NUMBER = RANGE('0' to '9')

val PARENTHESIS = RANGE(Set('(', ')', '{', '}'))
// Note: "Parenthesis" is the singular form of "parentheses."

val SYMBOL = (LETTER | RANGE(Set('.', '_', '>', '<', '=', ';', ',', '\\', ':', '!', '?', '&', '$', '#')) | AOP | PARENTHESIS)

val COLON = (CHAR(';') | CHAR(':'))

val WHITESPACE = PLUS(" " | "\n" | "\t" | "\r")

val IDENTIFIER = (LETTER ~ STAR(CHAR('_') | LETTER | NUMBER))

val INT = (OPT(CHAR('-')) ~ (CHAR('0') | (RANGE('1' to '9') ~ NUMBER.%)))

val DOUBLE = (INT ~ CHAR('.') ~ (PLUS(NUMBER)))

val COMMENT = "//" ~ STAR(SYMBOL | RANGE(Set(' ', '\"', '\'')) | NUMBER) ~ "\n"

val STRING = (CHAR('\"') ~ (SYMBOL | WHITESPACE | NUMBER).% ~ CHAR('\"'))

val CHARACTER = (CHAR('\'') ~ (SYMBOL | NUMBER | "\\n" | RANGE(Set(' ', '\n', '\t', '\r'))) ~ CHAR('\''))

val COMMA = CHAR(',')

// End of Question 1

val FUN_REGS = (	("key" $ KEYWORD) | 
                  ("id" $ IDENTIFIER) | 
                  ("aop" $ AOP) |
                  ("bop" $ BOP) | 
                  ("int" $ INT) |
                  ("db" $ DOUBLE) |
                  ("colon" $ COLON) | 
                  ("str" $ STRING) |
                  ("par" $ PARENTHESIS) | 
                  ("space" $ WHITESPACE) |
                  ("cmt" $ COMMENT) |
                  ("char" $ CHARACTER) |
                  ("comma" $ COMMA)).%

abstract class Token 

case class T_COLON(s: String) extends Token
case class T_LPAREN(s: String) extends Token
case class T_RPAREN(s: String) extends Token
case class T_ID(s: String) extends Token
case class T_AOP(s: String) extends Token
case class T_BOP(s: String) extends Token
case class T_INT(n: Int) extends Token
case class T_DB(d: Double) extends Token
case class T_KWD(s: String) extends Token
case class T_STR(s: String) extends Token
case class T_COMMENT(s: String) extends Token
case class T_CHAR(c: Char) extends Token
case object T_COMMA extends Token

val token : PartialFunction[(String, String), Token] = {
  case ("colon", s) => T_COLON(s)
  case ("par", "{") => T_LPAREN("{")
  case ("par", "(") => T_LPAREN("(")
  case ("par", "}") => T_RPAREN("}")
  case ("par", ")") => T_RPAREN(")")
  case ("id", s) => T_ID(s)
  case ("aop", s) => T_AOP(s)
  case ("bop", s) => T_BOP(s)
  case ("int", s) => T_INT(s.toInt)
  case ("db", s) => T_DB(s.toDouble)
  case ("key", s) => T_KWD(s)
  case ("str", s) => T_STR(s.filter(c => c != '\"'))
  case ("char", s) => T_CHAR(s.filter(c => c != '\'').replace("\\n", "\n").head)
  case ("comma", s) => T_COMMA
  // case ("com", s) => T_COMMENT(s)
}

// by using collect we filter out all unwanted tokens
def tokenise(s: String) : List[Token] = 
  lexing_simp(FUN_REGS, s).collect(token)




// END OF LEXER




//




// START OF PARSER




case class ~[+A, +B](x: A, y: B)

// constraint for the input
type IsSeq[A] = A => Seq[_]

abstract class Parser[I : IsSeq, T]{
  def parse(in: I): Set[(T, I)]

  def parse_all(in: I) : Set[T] =
    for ((hd, tl) <- parse(in); 
        if tl.isEmpty) yield hd
}

case class TokenParser(t: Token) extends Parser[List[Token], Token] {
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
			case T_KWD("Int") :: ts => Set(("Int", ts))
			case T_KWD("Double") :: ts => Set(("Double", ts))
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

case object CharParser extends Parser[List[Token], Int] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_CHAR(c) :: ts => Set((c.toInt, ts))
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






// the abstract syntax trees for the FUN language


abstract class Exp
abstract class BExp
abstract class Decl

case class Def(name: String, args: List[(String, String)], ty: String, body: Exp) extends Decl
// case class VoidDef(name: String, args: List[(String, String)], body: Exp) extends Decl
case class Main(e: Exp) extends Decl
case class Const(name: String, v: Int) extends Decl
case class FConst(name: String, x: Double) extends Decl

case class Call(name: String, args: List[Exp]) extends Exp
case class If(a: BExp, e1: Exp, e2: Exp) extends Exp
case class Var(s: String) extends Exp
case class Num(i: Int) extends Exp
case class FNum(i: Double) extends Exp
case class ChConst(c: Int) extends Exp
case class Aop(o: String, a1: Exp, a2: Exp) extends Exp
case class Sequence(e1: Exp, e2: Exp) extends Exp
case class Bop(o: String, a1: Exp, a2: Exp) extends BExp

case object True extends BExp
case object False extends BExp




lazy val Block: Parser[List[Token], Exp] = {
	(TokenParser(T_LPAREN("{")) ~ Block ~ TokenParser(T_RPAREN("}"))).map[Exp]{case _ ~ b ~ _ => b} ||
	(Exp ~ TokenParser(T_COLON(";")) ~ Block).map[Exp]{case e1 ~ _ ~ e2 => Sequence(e1, e2)} ||
	Exp
}

lazy val Exp: Parser[List[Token], Exp] = {
	(IdParser ~ TokenParser(T_LPAREN("(")) ~ Params ~ TokenParser(T_RPAREN(")"))).map[Exp]{case f ~ _ ~ ps ~ _ => Call(f, ps)} ||
	(IdParser ~ TokenParser(T_LPAREN("(")) ~ TokenParser(T_RPAREN(")"))).map[Exp]{case f ~ _ ~ _ => Call(f, Nil)} ||
	(TokenParser(T_KWD("if")) ~ BExp ~ TokenParser(T_KWD("then")) ~ Block ~ TokenParser(T_KWD("else")) ~ Block).map[Exp]{case _ ~ b ~ _ ~ e1 ~ _ ~ e2 => If(b, e1, e2)} ||
	SkipParser.map[Exp]{s => s} ||
	AExp
}

lazy val Params: Parser[List[Token], List[Exp]] = {
	(Exp ~ TokenParser(T_COMMA) ~ Params).map[List[Exp]]{case p1 ~ _ ~ ps => p1 :: ps} ||
	Exp.map[List[Exp]]{a => List(a)}
}

lazy val SkipParser: Parser[List[Token], Exp] = {
	(TokenParser(T_KWD("skip")) ~ TokenParser(T_LPAREN("(")) ~ TokenParser(T_RPAREN(")"))).map[Exp]{case _ => Call("skip", Nil)} ||
	(TokenParser(T_KWD("skip"))).map[Exp]{case _ => Call("skip", Nil)}
}




// arithmetic expressions
lazy val AExp: Parser[List[Token], Exp] = {
  (Term ~ TokenParser(T_AOP("+")) ~ AExp).map[Exp]{ case x ~ _ ~ z => Aop("+", x, z) } ||
  (Term ~ TokenParser(T_AOP("-")) ~ AExp).map[Exp]{ case x ~ _ ~ z => Aop("-", x, z) } || 
  Term
}
  
lazy val Term: Parser[List[Token], Exp] = {
  (Factor ~ TokenParser(T_AOP("*")) ~ Term).map[Exp]{ case x ~ _ ~ z => Aop("*", x, z) } || 
  (Factor ~ TokenParser(T_AOP("/")) ~ Term).map[Exp]{ case x ~ _ ~ z => Aop("/", x, z) } ||
  (Factor ~ TokenParser(T_AOP("%")) ~ Term).map[Exp]{ case x ~ _ ~ z => Aop("%", x, z) } || 
  Factor
} 
  
lazy val Factor: Parser[List[Token], Exp] = {
   (TokenParser(T_LPAREN("(")) ~ AExp ~ TokenParser(T_RPAREN(")"))).map{ case _ ~ y ~ _ => y } ||
   CallParser.map[Exp]{c => c} ||
   IdParser.map(Var) || IntParser.map(Num) || DoubleParser.map(FNum) ||
   CharParser.map(ChConst)
}
   
lazy val CallParser: Parser[List[Token], Exp] = {
	(IdParser ~ TokenParser(T_LPAREN("(")) ~ Params ~ TokenParser(T_RPAREN(")"))).map[Exp]{case f ~ _ ~ ps ~ _ => Call(f, ps)} ||
	(IdParser ~ TokenParser(T_LPAREN("(")) ~ TokenParser(T_RPAREN(")"))).map[Exp]{case f ~ _ ~ _ => Call(f, Nil)}
} 




// boolean expressions with some simple nesting
lazy val BExp: Parser[List[Token], BExp] = {
   (AExp ~ TokenParser(T_BOP("==")) ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("==", x, z) } || 
   (AExp ~ TokenParser(T_BOP("!=")) ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("!=", x, z) } || 
   (AExp ~ TokenParser(T_BOP("<")) ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<", x, z) } ||
   (AExp ~ TokenParser(T_BOP("<=")) ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<=", x, z) } ||
   (AExp ~ TokenParser(T_BOP(">")) ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<", z, x) } ||
   (AExp ~ TokenParser(T_BOP(">=")) ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<=", z, x) } ||
   (TokenParser(T_KWD("true"))).map[BExp]{ _ => True } || 
   (TokenParser(T_KWD("false"))).map[BExp]{ _ => False } ||
   (TokenParser(T_LPAREN("(")) ~ BExp ~ TokenParser(T_RPAREN(")"))).map[BExp]{ case _ ~ x ~ _ => x }
}




lazy val Decl: Parser[List[Token], Decl] = {

//	(TokenParser(T_KWD("def")) ~ IdParser ~ TokenParser(T_LPAREN("(")) ~ DefParams ~ TokenParser(T_RPAREN(")")) ~ TokenParser(T_COLON(":")) ~ TokenParser(T_KWD("Void")) ~ TokenParser(T_AOP("=")) ~ Block).map[Decl]{case _ ~ f ~ _ ~ dps ~ _ ~ _ ~ _ ~ _ ~ es => VoidDef(f, dps, es)} ||
//	(TokenParser(T_KWD("def")) ~ IdParser ~ TokenParser(T_LPAREN("(")) ~ TokenParser(T_RPAREN(")")) ~ TokenParser(T_COLON(":")) ~ TokenParser(T_KWD("Void")) ~ TokenParser(T_AOP("=")) ~ Block).map[Decl]{case _ ~ f ~ _ ~ _ ~ _ ~ _ ~ _ ~ es => VoidDef(f, Nil, es)} ||
	(TokenParser(T_KWD("def")) ~ IdParser ~ TokenParser(T_LPAREN("(")) ~ DefParams ~ TokenParser(T_RPAREN(")")) ~ TokenParser(T_COLON(":")) ~ TypeParser ~ TokenParser(T_AOP("=")) ~ Block).map[Decl]{case _ ~ f ~ _ ~ dps ~ _ ~ _ ~ t ~ _ ~ es => Def(f, dps, t, es)} ||
	(TokenParser(T_KWD("def")) ~ IdParser ~ TokenParser(T_LPAREN("(")) ~ TokenParser(T_RPAREN(")")) ~ TokenParser(T_COLON(":")) ~ TypeParser ~ TokenParser(T_AOP("=")) ~ Block).map[Decl]{case _ ~ f ~ _ ~ _ ~ _ ~ t ~ _ ~ es => Def(f, Nil, t, es)} ||
	(TokenParser(T_KWD("val")) ~ IdParser ~ TokenParser(T_COLON(":")) ~ TokenParser(T_KWD("Int")) ~ TokenParser(T_AOP("=")) ~ IntParser).map[Decl]{case _ ~ id ~ _ ~ t ~ _ ~ v => Const(id, v)} ||
	(TokenParser(T_KWD("val")) ~ IdParser ~ TokenParser(T_COLON(":")) ~ TokenParser(T_KWD("Double")) ~ TokenParser(T_AOP("=")) ~ DoubleParser).map[Decl]{case _ ~ id ~ _ ~ t ~ _ ~ x => FConst(id, x)} ||
	Block.map[Decl]{b => Main(b)}
	
	// (IdParser ~ TokenParser(T_LPAREN("(")) ~ Params ~ TokenParser(T_RPAREN(")"))).map[Decl]{case f ~ _ ~ ps ~ _ => Main(Call(f, ps))} ||
	// (IdParser ~ TokenParser(T_LPAREN("(")) ~ TokenParser(T_RPAREN(")"))).map[Decl]{case f ~ _ ~ _ => Main(Call(f, Nil))}
	
}

lazy val DefParams: Parser[List[Token], List[(String, String)]] = {
	(Arg ~ TokenParser(T_COMMA) ~ DefParams).map[List[(String, String)]]{case a ~ _ ~ dps => a :: dps} || Arg.map{a => List(a)}
}

lazy val Arg: Parser[List[Token], (String, String)] = {
	(IdParser ~ TokenParser(T_COLON(":")) ~ TypeParser).map[(String, String)]{case n ~ _ ~ t => (n, t)}
}




/*

*/




lazy val Prog: Parser[List[Token], (List[Decl], (List[String], List[String]))] = {
	(Decl ~ TokenParser(T_COLON(";")) ~ Prog).map{case Def(n, args, "Int", b) ~ _ ~ p => (Def(n, args, "Int", b) :: p._1, (n :: p._2._1, p._2._2)) ; case Def(n, args, "Double", b) ~ _ ~ p => (Def(n, args, "Double", b) :: p._1, (p._2._1, n :: p._2._2)) ; case Const(id, v) ~ _ ~ p => (Const(id, v) :: p._1, (id :: p._2._1, p._2._2)) ; case FConst(id, x) ~ _ ~ p => (FConst(id, x) :: p._1, (p._2._1, id :: p._2._2)) ; case d ~ _ ~ p => (d :: p._1, (p._2._1, p._2._2))} ||
	Decl.map{ case Def(n, args, "Int", b) => (List[Decl](Def(n, args, "Int", b)), (List(n), Nil)) ; case Def(n, args, "Double", b) => (List[Decl](Def(n, args, "Double", b)), (Nil, List(n))) ; case Const(id, v) => (List[Decl](Const(id, v)), (List(id), Nil)) ; case FConst(id, x) => (List[Decl](FConst(id, x)), (Nil, List(id))) ; case d => (List[Decl](d), (Nil, Nil)) }
}


 

def lex_fun(code: String) : List[Token] = tokenise(code)


// Call parse_fun(your_code_as_string) to obtain the abstract syntax tree.
def parse_fun(code: String) : (List[Decl], (List[String], List[String])) = {
	Prog.parse_all(tokenise(code)).head
}




// END OF PARSER AND INTERPRETER




// 




// START OF COMPILER






// for generating new labels
var counter = -1

def Fresh(x: String) = {
  counter += 1
  x ++ "_" ++ counter.toString()
}







// Internal CPS language for FUN
abstract class KExp
abstract class KVal

case class KVar(s: String) extends KVal
case class KNum(i: Int) extends KVal
case class KFNum(db: Double) extends KVal
case class Kop(o: String, v1: KVal, v2: KVal) extends KVal
case class KCall(o: String, vrs: List[KVal]) extends KVal
case class KVoid(c: KVal) extends KExp
case class KWrite(v: KVal) extends KVal

case class KLet(x: String, e1: KVal, e2: KExp) extends KExp {
  override def toString = s"LET $x = $e1 in \n$e2" 
}


case class KIf(x1: String, e1: KExp, e2: KExp) extends KExp {
  def pad(e: KExp) = e.toString.replaceAll("(?m)^", "  ")

  override def toString = 
     s"IF $x1\nTHEN\n${pad(e1)}\nELSE\n${pad(e2)}"
}
case class KReturn(v: KVal) extends KExp










// CPS translation from Exps to KExps using a
// continuation k.
def CPS(e: Exp, lists: (List[String], List[String]))(k: KVal => KExp) : KExp = e match {
  case Var(s) => k(KVar(s)) 
  case Num(i) => k(KNum(i))
  case FNum(i) => k(KFNum(i))
  case ChConst(i) => k(KNum(i))
  
  case Aop(o, e1, e2) => {
    val z = Fresh("tmp")
    CPS(e1, lists)(y1 => 
      CPS(e2, lists)(y2 => KLet(z, Kop(o, y1, y2), k(KVar(z)))))
  }
  
  case If(Bop(o, b1, b2), e1, e2) => {
    val z = Fresh("tmp")
    CPS(b1, lists)(y1 => 
      CPS(b2, lists)(y2 => 
        KLet(z, Kop(o, y1, y2), KIf(z, CPS(e1, lists)(k), CPS(e2, lists)(k)))))
  }
  
  case Call(name, args) => {
  	if ((!lists._1.contains(name)) && (!lists._2.contains(name))) {
  		def aux(args: List[Exp], vs: List[KVal]) : KExp = args match {
				case Nil => {
						KVoid(KCall(name, vs))
				}
				case e::es => CPS(e, lists)(y => aux(es, vs ::: List(y)))
			}
			aux(args, Nil)
  	}
  	else {
			def aux(args: List[Exp], vs: List[KVal]) : KExp = args match {
				case Nil => {
						val z = Fresh("tmp")
						KLet(z, KCall(name, vs), k(KVar(z)))
				}
				case e::es => CPS(e, lists)(y => aux(es, vs ::: List(y)))
			}
			aux(args, Nil)
		}
  }
  
  case Sequence(e1, e2) => CPS(e1, lists)(_ => CPS(e2, lists)(y2 => k(y2)))
}   


//initial continuation
def CPSi(e: Exp, lists: (List[String], List[String])) = CPS(e, lists)(KReturn)




// convenient string interpolations 
// for instructions, labels and methods
import scala.language.implicitConversions
import scala.language.reflectiveCalls

implicit def sring_inters(sc: StringContext) = new {
    def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
    def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
    def m(args: Any*): String = sc.s(args:_*) ++ "\n"
}


// mathematical and boolean operations
def compile_int_op(op: String) = op match {
  case "+" => "add"
  case "*" => "mul"
  case "-" => "sub"
  case "/" => "sdiv"
  case "%" => "srem"
  case "==" => "icmp eq"
  case "<=" => "icmp sle"     // signed less or equal
  case "<"  => "icmp slt"     // signed less than
}




// compile K values
def compile_val(v: KVal, lists: (List[String], List[String]), ilist: List[String]) : String = v match {
  case KNum(i) => s"i32 $i"
  case KFNum(f) => s"double $f"
  case KVar(s) => {
  	if (lists._1.contains(s)) s"i32 @$s"
  	else if (lists._2.contains(s)) s"double @$s"
  	else if (ilist.contains(s)) s"i32 %$s"
  	else s"double %$s"
  }
  case Kop(op, x1, x2) => 
    s"${compile_int_op(op)} ${compile_val(x1, lists, ilist)}, ${compile_val(x2, lists, ilist).split(' ').last}"
  
  case KCall(x1, args) => 
  	if (lists._1.contains(x1)) {
  		s"call i32 @$x1 (${args.map(a => compile_val(a, lists, ilist)).mkString("", ", ", "")})"
  	}
  	else if (lists._2.contains(x1)) {
  	  s"call double @$x1 (${args.map(a => compile_val(a, lists, ilist)).mkString("", ", ", "")})"
  	}
  	else {
  		s"call void @$x1 (${args.map(a => compile_val(a, lists, ilist)).mkString("", ", ", "")})"
  	}
}

// compile K expressions
def compile_exp(a: KExp, lists: (List[String], List[String]), ilist: List[String]) : String = a match {
  case KReturn(v) => i"ret ${compile_val(v, lists, ilist)}"
  case KLet(x: String, v: KVal, e: KExp) => 
    i"%$x = ${compile_val(v, lists, ilist)}" ++ compile_exp(e, lists, ilist)
  case KIf(x, e1, e2) => {
    val if_br = Fresh("if_branch")
    val else_br = Fresh("else_branch")
    i"br i1 %$x, label %$if_br, label %$else_br" ++
    l"\n$if_br" ++
    compile_exp(e1, lists, ilist) ++
    l"\n$else_br" ++ 
    compile_exp(e2, lists, ilist)
  }
  case KVoid(v) => {
  	i"${compile_val(v, lists, ilist)}"
  }
}


val prelude = """
@.str = private constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i8*, ...)

define i32 @printInt(i32 %x) {
   %t0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
   call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
   ret i32 %x
}

"""


// compile function for declarations and main
def compile_decl(d: Decl, lists: (List[String], List[String])) : String = d match {
  case Def(name, args, tp, body) => { 
  	def process_params(params: List[(String, String)]) : (String, List[String]) = params match {
  		case Nil => ("", Nil)
  		case (p, "Int") :: Nil => {
  			val nn = Fresh(p)
  			("i32 " ++ nn, List(p))
  		}
  		case (p, "Double") :: Nil => {
  			val nn = Fresh(p)
  			("double " ++ nn, List(p))
  		}
  		case (p, "Int") :: ps => {
  			val (r, l) = process_params(ps)
  			val nn = Fresh(p)
  			("i32 " ++ nn ++ ", " ++ r, p :: l)
  		}
  		case (p, "Double") :: ps => {
  			val (r, l) = process_params(ps)
  			val nn = Fresh(p)
  			("double " ++ nn ++ ", " ++ r, p :: l)
  		}
  		case _ => ("", Nil)
  	}
  	def type_match(tpstr: String) : String = tpstr match {
  		case "Int" => "i32"
  		case "Double" => "double"
  		case _ => "void"
  	}
  	val (r, l) = process_params(args)
	  "define " ++ type_match(tp) ++ m" $name (" ++ r ++ m") {" ++
	  compile_exp(CPSi(body, lists), lists, l) ++
	  m"}"
  }
  case Const(id, v) => {
  	m"@$id = global i32 $v"
  }
  case FConst(id, v) => {
  	m"@id = global double $v"
  }
  case Main(body) => {
    m"define i32 @main() {" ++
    compile_exp(CPS(body, lists)(_ => KReturn(KNum(0))), lists, Nil) ++
    m"}\n"
  }
}




def compile_ast(ast: List[Decl], lists: (List[String], List[String])) : String = {
	prelude ++ (ast.map(a => compile_decl(a, lists)).mkString)
}


// main compiler functions
def compile(code: String) : String = {
	val (ast, lists) = parse_fun(code)
	compile_ast(ast, lists)
}


// pre-2.5.0 ammonite 
// import ammonite.ops._

// post 2.5.0 ammonite
// import os._




/*
@main
def main(fname: String) = {
    val path = os.pwd / fname
    val file = fname.stripSuffix("." ++ path.ext)
    val tks = tokenise(os.read(path))
    val ast = parse_tks(tks)
    println(compile(ast))
}

@main
def write(fname: String) = {
    val path = os.pwd / fname
    val file = fname.stripSuffix("." ++ path.ext)
    val tks = tokenise(os.read(path))
    val ast = parse_tks(tks)
    val code = compile(ast)
    os.write.over(os.pwd / (file ++ ".ll"), code)
}

@main
def run(fname: String) = {
    val path = os.pwd / fname
    val file = fname.stripSuffix("." ++ path.ext)
    write(fname)  
    os.proc("llc", "-filetype=obj", file ++ ".ll").call()
    os.proc("gcc", file ++ ".o", "-o", file ++ ".bin").call()
    os.proc(os.pwd / (file ++ ".bin")).call(stdout = os.Inherit)
    println(s"done.")
}
*/

















