
// package jucheparse

object LexerGenerator {

	def traverse_exp(e: Grammar.Exp) : List[String] = e match {
		case Grammar.Keyword(s) => List(s)
		case Grammar.AltExp(e1, e2) => traverse_exp(e1) ::: traverse_exp(e2)
		case Grammar.CardiExp(e, _) => traverse_exp(e)
		case Grammar.SeqExp(es) => traverse_exps(es)
		case _ => Nil
	}

	def traverse_exps(el: List[Grammar.Exp]) : List[String] = el match {
		case e :: es => traverse_exp(e) ::: traverse_exps(es)
		case Nil => Nil
	}

	def traverse_elems(el: List[Grammar.Elem]) : List[String] = el match {
		case e :: es => e.s :: traverse_elems(es)
		case Nil => Nil
	}

	def traverse_stmt(s: Grammar.Stmt) : List[String] = s match {
		case Grammar.Program(_, el) => traverse_exps(el)
		case Grammar.Rule(_, el) => traverse_exps(el)
		case Grammar.Enumerate(_, el) => traverse_elems(el)
		case _ => Nil
	}

	def reg_to_rexp(reg: String) : Rexp = {
		val r = RegexParser.parse(reg)
		if (r == ZERO) ONE
		else r
	}

	def build_alt_keys(xs: List[String]) : String = xs match {
		case Nil => "ZERO"
		case k :: Nil => s""""${k}""""
		case k :: ks => s""""${k}" | ${build_alt_keys(ks)}"""
	}

	def stmts_to_terminals(ls: List[Grammar.Stmt]) : List[Grammar.Terminal] = ls match {
		case Nil => Nil
		case t :: sx if t.isInstanceOf[Grammar.Terminal] => t.asInstanceOf[Grammar.Terminal] :: stmts_to_terminals(sx)
		case s :: sx => stmts_to_terminals(sx)
	}

	def select_fragments(ls: List[Grammar.Terminal]) : List[Grammar.Terminal] = ls match {
		case Nil => Nil
		case t :: sx if (t.frag == true) =>
			t :: select_fragments(sx)
		case t :: sx => select_fragments(sx)
	}

	def select_non_fragments(ls: List[Grammar.Terminal]) : List[Grammar.Terminal] = ls match {
		case Nil => Nil
		case t :: sx if (t.frag == false) =>
			t :: select_non_fragments(sx)
		case t :: sx => select_non_fragments(sx)
	}

	def terminals_def_as_string(tl: List[Grammar.Terminal]) : String = tl match {
		case Nil => ""
		case t :: ts => {
			s"""val ${t.id} = ${t.pat}
			|""".stripMargin ++ terminals_def_as_string(ts)
		}
	}

	def find_types_in_exps(el: List[Grammar.Exp]) : List[String] = el match {
		case Nil => Nil
		case Grammar.TypeExp(t) :: es => t :: find_types_in_exps(es)
		case e :: es => find_types_in_exps(es)
	}
	
	def find_types_in_stmts(ls: List[Grammar.Stmt]) : List[String] = ls match {
		case Nil => Nil
		case Grammar.Rule(_, exps) :: xs => find_types_in_exps(exps) ::: find_types_in_stmts(xs)
		case Grammar.Program(_, exps) :: xs => find_types_in_exps(exps) ::: find_types_in_stmts(xs)
		case s :: xs => find_types_in_stmts(xs)
	}

	def build_types(sl: List[String]) : String = sl match {
		case Nil => ""
		case "ID" :: xs => {
			"""val IDENTIFIER = (LETTER ~ STAR(CHAR('_') | LETTER | |NUMBER))
			|""".stripMargin ++ build_types(xs)
		}
		case "INT" :: xs => {
			"""val INT = (OPT(CHAR('-')) ~ (CHAR('0') | (RANGE('1' to '9') ~ NUMBER.%)))
			|""".stripMargin ++ build_types(xs)
		}
		case "DOUBLE" :: xs => {
			"""val DOUBLE = (INT ~ CHAR('.') ~ (PLUS(NUMBER)))
			|""".stripMargin ++ build_types(xs)
		}
		case "STRING" :: xs => {
			"""val STRING = (CHAR('\"') ~ STAR(SYMBOL | '\'') ~ CHAR('\"'))
			|""".stripMargin ++ build_types(xs)
		}
		case "CHAR" :: xs => {
			"""val CHARACTER = (CHAR('\'') ~ (SYMBOL | '\"') ~ CHAR('\''))
			|""".stripMargin ++ build_types(xs)
		}
		case _ :: xs => ""
	}

	def build_types_as_recd_rexp(sl: List[String]) : List[String] = sl match {
		case Nil => Nil
		case "ID" :: ts => s"""("id" $$ ID)""" :: build_types_as_recd_rexp(ts)
		case "INT" :: ts => s"""("int" $$ INT)""" :: build_types_as_recd_rexp(ts)
		case "DOUBLE" :: ts => s"""("db" $$ DOUBLE)""" :: build_types_as_recd_rexp(ts)
		case "STRING" :: ts => s"""("str" $$ STRING)""" :: build_types_as_recd_rexp(ts)
		case "CHAR" :: ts => s"""("char" $$ CHARACTER)""" :: build_types_as_recd_rexp(ts)
		case t :: ts => build_types_as_recd_rexp(ts)
	}

	val template = LexerTemplate.get

	def generate(ls: List[Grammar.Stmt]) : String = {
		val title = ls match {
			case t :: sx if (t.isInstanceOf[Grammar.Title]) => t.asInstanceOf[Grammar.Title].p
			case _ => "not_named"
		}

		val types = find_types_in_stmts(ls)

		val types_def = build_types(types)

		val terminals = stmts_to_terminals(ls)

		val terminals_def = terminals_def_as_string(terminals)

		val kwds = ls.flatMap(traverse_stmt).distinct

		val kwds_words = kwds.filter(k => k.size > 1)

		val kwds_chars = kwds.filter(k => k.size == 1)

		val kwds_words_reg = s"""("key" $$ ${build_alt_keys(kwds)})"""

		def print_chars(sc: List[String]) : String = sc match {
			case Nil => ""
			case s :: Nil => s"\'${s}\'"
			case s :: sx => s"\'${s}\', " ++ print_chars(sx)
		}

		val kwds_chars_reg = s"""("sym" $$ RANGE(Set[Char](${print_chars(kwds_chars)})))"""

		val types_recds = {
			val rxds = build_types_as_recd_rexp(types)
			if (! rxds.isEmpty) rxds.mkString("\n", "|\n", "|\n")
			else ""
		}

		val non_fragments = select_non_fragments(terminals)

		val non_fragments_cases_list = non_fragments.map[String](t => s"""case ("${t.id}", s) => T_${t.id}(s)""")

		val terminal_recds_list = non_fragments.map(t => s"""("${t.id}" $$ ${t.id})""")

		val terminal_recds = {
			if (! terminal_recds_list.isEmpty) terminal_recds_list.mkString("(", "|\n", ")|\n")
			else ""
		}

		val terminals_tokens = non_fragments.map(t => s"""case class T_${t.id}(s: String) extends Token""").mkString("", "\n", "")

		val hiddens = ls.filter(s => s.isInstanceOf[Grammar.Hidden]).map[Grammar.Hidden](s => s.asInstanceOf[Grammar.Hidden])

		val hidden_recds_list = hiddens.map(h => s"""("${h.id}" $$ ${h.pat})""")

		val hidden_recds = {
			if (! hidden_recds_list.isEmpty) hidden_recds_list.mkString("|\n(", "|\n", ")")
			else ""
		}

		val define_lang : String = {
			s"""val LANGUAGE_REG = {
			|	STAR(
			|		${kwds_words_reg} |
			|		${kwds_chars_reg} | ${types_recds} ${terminal_recds}
			|		("ws" $$ WHITESPACE) ${hidden_recds}
			|	)
			|}"""
		}

		val terminal_cases = {
			if (! non_fragments_cases_list.isEmpty)
				non_fragments_cases_list.mkString("", "\n", "")
			else ""
		}

		val token_partial_function = """
		|val token : PartialFunction[(String, String), Token] = {
		|	case ("key", s) => T_KEY(s)
		|	case ("sym", c) =>
		|		try {T_SYM(s.head)}
		|		catch {case e: Exception => T_SYM('?')}
		|	case ("id", s) => T_ID(s)
		|	case ("int", s) =>
		|		try {T_INT(s.toInt)}
		|		catch {case e: Exception => T_INT(0)}
		|	case ("db", s) =>
		|		try {T_DB(s.toDouble)}
		|		catch {case e: Exception => T_DB(0.0)}
		|	case ("str", s) =>
		|		try {
		|			val s2 = s.init.tail
		|			val s3 = process_string(s2.toList).mkString
		|			T_STR(s3)
		|		} catch {
		|			case e: Exception => T_STR("")
		|		}
		|	case ("char", s) =>
		|		try {
		|			val s2 = s.init.tail
		|			val c = process_string(s2.toList).head
		|			T_CHAR(c)
		|		} catch {
		|			case e: Exception => T_CHAR('?')
		|		}
		|	${terminal_cases}
		|}"""

		// Below is the return value

		s"""
		|package ${title}
		|
		|${template}
		|
		|object ${title}Tokenizer {
		|
		|val LETTER = RANGE((('a' to 'z') ++ ('A' to 'Z')).toSet)
		|
		|val NUMBER = RANGE('0' to '9')
		|
		|val WHITESPACE = PLUS(" " | "\\n" | "\\t" | "\\r")
		|
		|val SYMBOL = (
		|	LETTER | NUMBER |
		|	RANGE(Set('+', '-', '*', '/', '%', '=', '>', '<', '.', '_', ',', ';', ':', '!', '?', '|', '&', '~','$$', '#', '^', '`', '@', '(', ')', '{', '}', '[', ']', ' ')) |
		|	(CHAR('\\\\') ~ RANGE(Set('\\\\', '\\"', '\\'', 'n', 't', 'r')))
		|)
		|
		|${types_def}
		|
		|${terminals_def}
		|
		|${define_lang}
		|
		|abstract class Token
		|
		|case class T_KEY(s: String) extends Token
		|case class T_SYMBOL(c: Char) extends Token
		|case class T_ID(s: String) extends Token
		|case class T_INT(n: Int) extends Token
		|case class T_DB(d: Double) extends Token
		|case class T_STR(s: String) extends Token
		|case class T_CHAR(c: Char) extends Token
		|
		|${terminals_tokens}
		|
		|def process_string(s: List[Char]) : List[Char] = s match {
		|case '\\\\' :: '\\\\' :: cs => '\\' :: process_string(cs)
		|case '\\\\' :: '\\\"' :: cs => '\\\"' :: process_string(cs)
		|case '\\\\' :: '\\\'' :: cs => '\\\'' :: process_string(cs)
		|case '\\\\' :: 'n' :: cs => '\\n' :: process_string(cs)
		|case '\\\\' :: 't' :: cs => '\\t' :: process_string(cs)
		|case '\\\\' :: 'r' :: cs => '\\r' :: process_string(cs)
		|case c :: cs => c :: process_string(cs)
		|case Nil => Nil
		|}
		|
		|${token_partial_function}
		|
		|def tokenize(s: String) : List[Token] = {
  	|	lex(LANGUAGE_REG, s).collect(token)
		|}
		|
		|}
		|
		""".stripMargin
	}

}
