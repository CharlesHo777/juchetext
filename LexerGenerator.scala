
// package jucheparse

class LexerGenerator {

	def traverse_exp(e: Grammar.Exp) : List[String] = e match {
		case Grammar.Keyword(s) => List(s)
		case Grammar.CallRule(_) => Nil
		case Grammar.AltExp(e1, e2) => traverse_exp(e1) ::: traverse_exp(e2)
		case Grammar.TypeExp(_) => Nil
		case Grammar.CardiExp(e, _) => traverse_exp(e)
		case Grammar.NewLine => Nil
	}

	def traverse_exps(el: List[Grammar.Exp]) : List[String] = el match {
		case e :: es => traverse_exp(e) :: traverse_exps(es)
		case Nil => Nil
	}

	def traverse_stmt(s: Grammar.Stmt) : List[String] = s match {
		case Grammar.Program(_, el) => traverse_exps(el)
		case Grammar.Rule(_, el, _) => traverse_exps(el)
		case Grammar.Enumerate(_, sl, _) => sl.map(s => s match {case Grammar.SElem(_, v) => v ; case _ => ""})
		case Grammar.Terminal(_, _, _) => Nil
		case _ => Nil
	}
	
	val template = LexerTemplate.get

	def generate(ls: List[Grammar.Stmt]) : String = {

		s"""
		
		"""

	}

}
