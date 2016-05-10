package mlscala

import mlscala.Ast._

import scala.util.parsing.combinator._

object Parser extends RegexParsers {

  def parse(input: String) = parseAll(toplevel, input)

  lazy val reserved = IF | THEN | ELSE | LET | IN | FUN | AND | TRUE | FALSE
  lazy val IF = "if"
  lazy val THEN = "then"
  lazy val ELSE = "else"
  lazy val LET = "let"
  lazy val IN = "in"
  lazy val FUN = "fun"
  lazy val AND = "and"
  lazy val TRUE = "true"
  lazy val FALSE = "false"
  lazy val RARROW = "->"
  lazy val SEMISEMI = ";;"
  lazy val BAND = "&&"
  lazy val BOR = "||"
  lazy val EQUAL = "="
  lazy val LESS = "<"
  lazy val PLUS = "+"
  lazy val MULT = "*"

  lazy val toplevel = topExpr | topLet
  lazy val topExpr = expr <~ SEMISEMI ^^ { case e => Exp(e) }
  lazy val topLet = LET ~> rep1sep(binding, AND) <~ SEMISEMI ^^ { case bindings: List[Decl] => MultiDecl(bindings) }
  lazy val binding = (name <~ EQUAL) ~ expr ^^ { case id ~ e => Decl(id, e) }

  lazy val expr = ifExpr | letExpr | funExpr | andExpr

  lazy val andExpr = and | orExpr
  lazy val and = orExpr ~ BAND ~ orExpr ^^ { case p1 ~ _ ~ p2 => BinOp(And, p1, p2)}

  lazy val orExpr = or | ltExpr
  lazy val or = ltExpr ~ BOR ~ ltExpr ^^ { case p1 ~ _ ~ p2 => BinOp(Or, p1, p2)}

  lazy val ltExpr = lessthan | pExpr
  lazy val lessthan = pExpr ~ LESS ~ pExpr ^^ { case p1 ~ _ ~ p2 => BinOp(Lt, p1, p2)}

  lazy val pExpr: Parser[Expr] = plus | mExpr
  lazy val plus = mExpr ~ PLUS ~ mExpr ^^ { case p1 ~ _ ~ p2 => BinOp(Plus, p1, p2) }

  lazy val mExpr: Parser[Expr] = mult | appExpr
  lazy val mult = appExpr ~ MULT ~ appExpr ^^ { case p1 ~ _ ~ p2 => BinOp(Mult, p1, p2) }

  lazy val appExpr: Parser[Expr] = application | aExpr
  lazy val application = aExpr ~ appExpr ^^ { case fun ~ arg => AppExp(fun, arg) }

  lazy val aExpr = intv | truev | falsev | varv | paren

  lazy val name = not(reserved) ~> """[a-zA-Z]+""".r
  lazy val intv = """\d+""".r ^^ { case n => ILit(n.toInt) }
  lazy val truev = "true" ^^ { case _ => BLit(true) }
  lazy val falsev = "false" ^^ { case _ => BLit(false) }
  lazy val varv = not(reserved) ~> """[a-zA-Z]+""".r ^^ { case s => Var(s) }
  lazy val paren = "(" ~> expr <~ ")"

  lazy val ifExpr: Parser[Expr] = (IF ~> expr <~ THEN) ~ expr ~ (ELSE ~> expr) ^^ {
    case cond ~ et ~ ef => IfExp(cond, et, ef)
  }

  lazy val letExpr: Parser[Expr] = LET ~> (name <~ EQUAL) ~ expr ~ (IN ~> expr) ^^ {
    case id ~ e ~ body => LetExp(id, e, body)
  }

  lazy val funExpr: Parser[Expr] = (FUN ~> rep1(name)) ~ (RARROW ~> expr) ^^ {
    case args ~ body => args.foldRight(body) { (arg, exp) => FunExp(arg, exp) }
  }
}
