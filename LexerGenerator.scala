

package juchetext


class LexerGenerator {


	abstract class Elem
	abstract class Exp
	abstract class Stmt

	case class Heading(kwd: String, p1: String, p2: String) extends Stmt

	case class Program(id: String, exps: List[Exp]) extends Stmt
	case class Rule(id: String, exps: List[Exp], mod: Modifier) extends Stmt
	case class Enumerate(id: String, el: List[Elem], mod: Modifier) extends Stmt
	case class Terminal(id: String, pat: Rexp, mod: Modifier) extends Stmt

	case class Keyword(s: String) extends Exp
	case class Assign(id: String, op: String, v: Exp) extends Exp
	case class CallRule(r: String) extends Exp
	case class AltExp(e1: Exp, e2: Exp) extends Exp
	case class SeqExp(e1: Exp, e2: Exp) extends Exp
	case class RefExp(r: String) extends Exp
	case class TypeExp(t: String) extends Exp
	case class CardiExp(e: Exp, c: Cardi) extends Exp
	case class Action(i: String) extends Exp

	// case class IElem(n: String, v: Int) extends Elem
	// case class DElem(n: String, v: Double) extends Elem
	// case class CElem(n: String, v: Char) extends Elem
	case class SElem(n: String, v: String) extends Elem

	abstract class Type
	case object IntType extends Type
	case object DoubleType extends Type
	case object StringType extends Type
	case object CharType extends Type
	case class TerminalType(t: Terminal) extends Type

	abstract class Cardi
	case object OptCardi extends Cardi
	case object PlusCardi extends Cardi
	case object StarCardi extends Cardi

	case class Modifier(component: Boolean, returns: String, hidden: List[String])

	
	val template = LexerTemplate.get


	def generate(ls: List[Stmt]) : String = {

		s"""
		
		"""
	}

}



