
// package jucheparse

object ParserGenerator {

def stmts_to_terminals(ls: List[Grammar.Stmt]) : List[Grammar.Terminal] = ls match {
	case Nil => Nil
	case t :: sx if t.isInstanceOf[Grammar.Terminal] => t.asInstanceOf[Grammar.Terminal] :: stmts_to_terminals(sx)
	case s :: sx => stmts_to_terminals(sx)
}

def select_non_fragments(ls: List[Grammar.Terminal]) : List[Grammar.Terminal] = ls match {
	case Nil => Nil
	case t :: sx if (t.frag == false) =>
		t :: select_non_fragments(sx)
	case t :: sx => select_non_fragments(sx)
}

def build_terminal_parsers(ls: List[Grammar.Stmt]) : String = {
	val non_fragments = select_non_fragments(stmts_to_terminals(ls))

	val parser_list = non_fragments.map(t => t.id).map(
		t_name => s"""
		|case object ${t_name}Parser extends Parser[List[Token], String] {
		|	def parse(tl: List[Token]) = {
		|		if (tl != Nil) tl match {
		|			case T_${t_name}(s) :: ts => Set((s, ts))
		|			case _ => Set()
		|		}
		|		else Set()
		|	}
		|}
		"""
	)
	parser_list.mkString("", "", "")
}

def have_assigns(el: List[Grammar.Exp]) : Boolean = el match {
	case Assign :: es => true
	case _ :: es => have_assigns(es)
	case Nil => false
}

def create_node(id: String) : String = {
	s"case class ${id}Node(ns: List[Node]) extends Node\n"
}

def create_nodes(ls: List[Grammar.Stmt]) : String = ls match {
	case Rule(id, _) :: sx => create_node(id) ++ create_nodes(sx)
	case Program(id, _) :: sx => create_node(id) ++ create_nodes(sx)
	case Enumerate(id, _) :: sx => s"case class ${id}Node(k: String) extends Node\n" ++ create_nodes(sx)
	case Nil => ""
}

def retrieve_exps_from_rule(r: Grammar.Rule) : List[Grammar.Exp] = {
	r.exps
}

def retrieve_exps_from_program(p: Grammar.Program) : List[Grammar.Exp] = {
	p.exps
}

def build_type_parser(t: String) : String = t match {
	case "ID" => "IdParser"
	case "INT" => "IntParser"
	case "DOUBLE" => "DoubleParser"
	case "STRING" => "StrParser"
	case "CHAR" => "CharParser"
	case _ => "NothingParser"
}

def build_cardi_parser(e: Grammar.Exp, c: Grammar.Cardi) : String = c match {
	case Grammar.C_OPT => s"CardiParser(${build_parser(e)}, C_OPT)"
	case Grammar.C_PLUS => s"CardiParser(${build_parser(e)}, C_PLUS)"
	case Grammar.C_STAR => s"CardiParser(${build_parser(e)}, C_STAR)"
	case _ => s"NothingParser"
}

def build_alt_parser(e1: Grammar.Exp, e2: Grammar.Exp) : String = {
	s"${build_parser(e1)} || ${build_parser(e2)}"
}

def build_seq_parser(el: List[Grammar.Exp]) : String = {
	if (! el.isEmpty)
		el.map(build_parser).mkString("", " ~ ", "")
	else
		"NothingParser"
}

case class Assign(e: Exp) extends Exp
case class CardiExp(e: Exp, c: Cardi) extends Exp
case class SeqExp(es: List[Exp]) extends Exp

def parse_results(el: List[Grammar.Exp], i: Int = 1) : List[String] = el match {
	case Assign :: es => s"n${i}" :: parse_results(es, i + 1)
	case _ :: es => "_" :: parse_results(es, i)
	case Nil => Nil
}

def print_parse_results(el: List[Grammar.Exp]) : (String, Int) = {
	val res = el.parse_results(el)
	val n = res.count(r => r != "_")
	if (! res.isEmpty)
		(res.mkString("", " ~ ", ""), n)
	else
		("_", 0)
}

def node_list(count: Int) : String = {
	val l = (1 to count).toList
	l.map(i => s"n${i}").mkString("", ",", "")
}

def build_parser(e: Grammar.Exp) : String = e match {
	case Keyword(s) => s"KWP(\"${s}\")"
	case CallRule(r) => s"${r}Parser"
	case Assign(e) => build_parser(e)
	case TypeExp(t) => build_type_parser(t)
	case CardiExp(e, c) => build_cardi_parser(e, c)
	case AltExp(e1, e2) => s"(${build_alt_parser(e1, e2)})"
	case SeqExp(es) => {
		val (res, n) = print_parse_results(es)
		val node = {
			if (n <= 0) "EmptyNode"
			else if (n == 1) "n1"
			else s"SeqNode(List(${node_list(n)}))"
		}
		s"(${build_seq_parser(es)}.map{case ${res} => ${node}})"
	}
	case _ => ""
}

def build_parser_sequence(el: List[Grammar.Exp]) : String = {
	val parser_list = el.map(build_parser)
	
	if (parser_list.size > 1)
		parser_list.mkString("(", " ~ ", ")")
	else if (parser_list.size == 1)
		parser_list.head
	else
		"NothingParser"
}

def list_elems(el: List[Grammar.Elem]) : String = {
	if (! el.isEmpty) {
		val elem_parsers = el.map(e => s"KWP(\"${e.s}\")")
		elem_parsers.mkString("", " || ", "")
	}
	else
		"NothingParser"
}

def build_stmt_parser(s: Grammar.Stmt) : String = {
	if (s.isInstanceOf[Grammar.Rule] {
		val r = s.asInstanceOf[Grammar.Rule]
		val el = r.exps
		val exps_seq = {
			if (! el.isEmpty)
				el.map(build_parser).mkString("", "~", "")
			else
				"NothingParser"
		}
		val (res, n) = print_parse_results(el)
		s"""
		|lazy val ${r.id}Parser: Parser[List[Token], Node] = {
		|	${exps_seq}.map{
		|		case ${res} => ${r.id}Node(List(${node_list(n)}))
		|	}
		|}
		|""".stripMargin
	}
	else if (s.isInstanceOf[Grammar.Program]) {
		val p = s.asInstanceOf[Grammar.Program]
		build_stmt_parser(Grammar.Terminal(p.id, p.exps))
	}
	else if (s.isInstanceOf[Grammar.Enumerate]) {
		val en = s.asInstanceOf[Grammar.Enumerate]
		val el = en.el
		s"""
		|lazy val ${en.id}Parser: Parser[List[Token], Node] = {
		|	(${list_elems(el)}).map[Node]{
		|		case s => ${en.id}Node(s)
		|	}
		|}
		"""
	}
	else ""
}

def create_nodes(ls: List[Grammar.Stmt]) : String = ls match {
	case Grammar.Program(id, es) :: sx => create_node(id, es) ++ create_nodes(sx)
	case Grammar.Rule(id, es) :: sx => create_node(id, es) ++ create_nodes(sx)
	case _ :: sx => create_nodes(sx)
	case Nil => ""
}

def generate(ls: List[Grammar.Stmt]) : String = {

val title = ls match {
	case t :: sx if (t.isInstanceOf[Grammar.Title]) => t.asInstanceOf[Grammar.Title].p
	case _ => "not_named"
}

s"""

package ${title}

object ${title}Parser {

case class TKP(t: Token) extends Parser[List[Token], Node] {
	def parse(in: List[Token]) = {
		if (in == Nil) Set()
		else if (in.head == t) Set((TokenNode(t), in.tail))
		else Set()
	}
}

case class KWP(k: String) extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case ${title}Tokenizer.T_KEY(s) :: ts if (k == s) Set((KeyNode(k), ts))
			case ${title}Tokenizer.T_SYM(s) :: ts if (k == s) Set((KeyNode(k), ts))
			case _ => Set()
		}
		else Set()
	}
}

case object IdParser extends Parser[List[Token], Node] {
  def parse(tl: List[Token]) = 
  	if (tl != Nil) tl match {
  		case ${title}Tokenizer.T_ID(s) :: ts => Set((IdNode(s), ts))
  		case _ => Set()
  	}
  	else Set()
}

case object IntParser extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case ${title}Tokenizer.T_INT(n) :: ts => Set((IntNode(n), ts))
			case _ => Set()
		}
		else Set()
	}
}

case object DoubleParser extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case ${title}Tokenizer.T_DB(db) :: ts => Set((DoubleNode(db), ts))
			case _ => Set()
		}
		else Set()
	}
}

case object StrParser extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case ${title}Tokenizer.T_STR(s) :: ts => Set((StringNode(s), ts))
			case _ => Set()
		}
		else Set()
	}
}

case object CharParser extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		if (tl != Nil) tl match {
			case ${title}Tokenizer.T_CHAR(c) :: ts => Set((CharNode(c), ts))
			case _ => Set()
		}
		else Set()
	}
}

case class CardiParser(p: Parser[List[Token], Node], c: Cardi) extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = c match {
		case C_OPT => {
			(NothingParser || p).map{
				case n => OptionNode(n)
			}.parse(tl)
		}
		case C_PLUS => {
			(p ~ CardiParser(p, Grammar.C_STAR)).map[Node]{
				case n1 ~ n2 => {
					if (n2.isInstanceOf[CardiNode])
						CardiNode(n1 :: n2.asInstanceOf[CardiNode].ns)
					else
						CardiNode(List[Node](n1))
				}
			}.parse(tl)
		}
		case C_STAR => {
			(NothingParser || (p ~ CardiParser(p, Grammar.C_STAR)).map[Node]{
				case n1 ~ n2 => {
					if (n2.isInstanceOf[CardiNode])
						CardiNode(n1 :: n2.asInstanceOf[CardiNode].ns)
					else
						CardiNode(List[Node](n1))
				}
			})
		}
	}
}

case object NothingParser extends Parser[List[Token], Node] {
	def parse(tl: List[Token]) = {
		Set((OptionNode(None), tl))
	}
}

${build_terminal_parsers(ls)}

abstract class Node

case class SeqNode(ns: List[Node]) extends Node
case class KeyNode(k: String) extends Node
case class OptionNode(n: Node) extends Node
case class CardiNode(ns: List[Node]) extends Node
case object EmptyNode extends Node

case class IdNode(s: String) extends Node
case class IntNode(n: Int) extends Node
case class DoubleNode(d: Double) extends Node
case class StringNode(s: String) extends Node
case class CharNode(c: Char) extends Node

abstract class Cardi // cardinality of an expression

case object C_OPT extends Cardi // optional
case object C_PLUS extends Cardi // one or more times
case class C_STAR extends Cardi // zero or more times

${create_nodes(ls)}

${ls.map(build_stmt_parser).mkString("", "", "")}

}

"""

}

}
