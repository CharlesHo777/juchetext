
// package jucheparse

object ParserGenerator {

case class TKP(t: Token) extends Parser[List[Token], Token] {
	def parse(in: List[Token]) = {
		if (in == Nil) Set()
		else if (in.head == t) Set((t, in.tail))
		else Set()
	}
}

case class KWP(k: String) extends Parser[List[Token], String] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case ${title}Parser.T_KEY(s) :: ts if (k == s) Set((s, ts))
			case ${title}Parser.T_SYM(s) :: ts if (k == s) Set((s, ts))
			case _ => Set()
		}
		else Set()
	}
}

case object IdParser extends Parser[List[Token], String] {
  def parse(tl: List[Token]) = 
  	if (tl != Nil) tl match {
  		case ${title}Parser.T_ID(s) :: ts => Set((s, ts))
  		case _ => Set()
  	}
  	else Set()
}

case object IntParser extends Parser[List[Token], Int] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case ${title}Parser.T_INT(n) :: ts => Set((n, ts))
			case _ => Set()
		}
		else Set()
	}
}

case object DoubleParser extends Parser[List[Token], Double] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case ${title}Parser.T_DB(db) :: ts => Set((db, ts))
			case _ => Set()
		}
		else Set()
	}
}

case object StrParser extends Parser[List[Token], String] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case ${title}Parser.T_STR(s) :: ts => Set((s, ts))
			case _ => Set()
		}
		else Set()
	}
}

case object CharParser extends Parser[List[Token], Char] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case ${title}Parser.T_CHAR(c) :: ts => Set((c, ts))
			case _ => Set()
		}
		else Set()
	}
}

case object ${terminal}Parser extends Parser[List[Token], String] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case T_${terminal}(s) :: ts => Set((s, ts))
			case _ => Set()
		}
		else Set()
	}
}

abstract class Cardi
case object C_OPT extends Cardi
case object C_PLUS extends Cardi
case object C_STAR extends Cardi
case class C_EXACT(n: Int) extends Cardi
case class C_MIN(min: Int) extends Cardi
case class C_MINMAX(min: Int, max: Int) extends Cardi

case object NothingParser extends Parser[List[Token], Option[String]] {
	def parse(tl: List[Token]) = {
		Set((None, tl))
	}
}

abstract class Node
case class StringContainer(s: String) extends Node
case class NotesContainer(ns: List[Node]) extends Node

def print_node_parameters(ids: List[String]) : String = {
	val args = ids.map(s => s"${s}: Node")
	args.mkString("(", ",", ")")
}

def create_node(id: String, es: List[Grammar.Exp]) : String = {
	val args_names = find_assign_ids(es).distinct
	if (args_names != Nil)
		s"case class ${id}${print_node_parameters(args_names)} extends Node"
	else s"case object ${id} extends Node"
}

def build_rule_parser(r: Grammar.Rule) : String = {
	s"""
	|lazy val ${r.id}: Parser[List[Token], Node] = {
	|	
	|}""".stripMargin
}

def build_parser_sequence(el: List[Grammar.Exp]) : String = {
	
}

def create_nodes(ls: List[Grammar.Stmt]) : String = ls match {
	case Grammar.Program(id, es) :: sx => create_node(id, es) ++ create_nodes(sx)
	case Grammar.Rule(id, es) :: sx => create_node(id, es) ++ create_nodes(sx)
	case _ :: sx => create_nodes(sx)
	case Nil => ""
}

def find_assign_ids(el: List[Grammar.Exp]) : List[String] = el match {
	case Grammar.Assign(id, _, _) :: es => id :: find_assign_ids(es)
	case Grammar.SeqExp(xe) :: es => find_assign_ids(xe) ::: find_assign_ids(es)
	case Grammar.CardiExp(e, cd) :: es => find_assign_ids(List[Grammar.Exp](e)) ::: find_assign_ids(es)
	case _ :: es => find_assign_ids(es)
	case Nil => Nil
}

def find_assign_ids_in_stmts(ls: List[Grammar.Stmt])

def generate(ls: List[Grammar.Stmt]) : String = {



}

}
