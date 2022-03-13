

package jucheparse


class LexerGenerator {



	def traverse_exp(e: Grammar.Exp) : List[String] = e match {
		case Keyword(s) => List(s)
		case Assign(_, _, v) => traverse_exp(v)
		case CallRule(_) => Nil
		case AltExp(e1, e2) => traverse_exp(e1) ::: traverse_exp(e2)
		case SeqExp(e1, e2) => traverse_exp(e1) ::: traverse_exp(e2)
		case RefExp(_) => Nil
		case TypeExp(_) => Nil
		case CardiExp(e, _) => traverse_exp(e)
		case Action(_) => Nil
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


	// case class Modifier(component: Boolean, returns: String, hidden: List[String])
		
	
	val template = LexerTemplate.get


	def generate(ls: List[Grammar.Stmt]) : String = {

		s"""
		
		"""

	}

}



