

package jucheparser


class LexerGenerator {

	def traverse_exp(e: Grammar.Exp) : List[String] = e match {
		case _ => Nil
	}

	def traverse_exps(el: List[Grammar.Exp]) : List[String] = el match {
		case e :: es => traverse_exp(e) :: traverse_exps(es)
		case Nil => Nil
	}

	def traverse_stmt(s: Grammar.Stmt) : List[String] = s match {
		case Program(_, el) => traverse_exps(el)
		case Rule(_, el, _) => traverse_exps(el)
		case Enumerate(_, sl, _) => sl.map(s => s match {case SElem(_, v) => v ; case _ => ""})
		case Terminal(_, _, _) => Nil
		case _ => Nil
	}

	/*

	abstract class Elem
	abstract class Exp

	case class Keyword(s: String) extends Exp
	case class Assign(id: String, op: String, v: Exp) extends Exp
	case class CallRule(r: String) extends Exp
	case class AltExp(e1: Exp, e2: Exp) extends Exp
	case class SeqExp(e1: Exp, e2: Exp) extends Exp
	case class RefExp(r: String) extends Exp
	case class TypeExp(t: String) extends Exp
	case class CardiExp(e: Exp, c: Cardi) extends Exp
	case class Action(i: String) extends Exp

	case class Modifier(component: Boolean, returns: String, hidden: List[String])

	*/
	
	
	val template = LexerTemplate.get


	def generate(ls: List[Grammar.Stmt]) : String = {

		s"""
		
		"""

	}

}



