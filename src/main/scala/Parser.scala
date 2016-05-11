package mlscala

import mlscala.Ast._

import scala.util.parsing.combinator._

object Parser extends RegexParsers {

  def parse(input: String) = parseAll(toplevel, input)
  def parseProgramFile(input: String) = parseAll(programFile, input)

  lazy val reserved = IF | THEN | ELSE | LET | IN | FUN | DFUN | AND | TRUE | FALSE
  lazy val IF = "if"
  lazy val THEN = "then"
  lazy val ELSE = "else"
  lazy val LET = "let"
  lazy val IN = "in"
  lazy val FUN = "fun"
  lazy val DFUN = "dfun"
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
  lazy val MINUS = "-"
  lazy val MULT = "*"

  lazy val programFile = rep1(toplevel)

  lazy val toplevel = topExpr | topLet | topDecl
  lazy val topExpr = expr <~ SEMISEMI ^^ { case e => Exp(e) }
  lazy val topLet = LET ~> rep1sep(binding, AND) <~ SEMISEMI ^^ { case bindings: List[Decl] => MultiDecl(bindings) }
  lazy val binding = (name <~ EQUAL) ~ expr ^^ { case id ~ e => Decl(id, e) }
  lazy val topDecl = (LET ~> name) ~ rep1(name) ~ (EQUAL ~> expr) <~ SEMISEMI ^^ {
    case id ~ args ~ body => MultiDecl(List(Decl(id, args.foldRight(body) { (arg, exp) => FunExp(arg, exp) })))
  }

  lazy val expr = ifExpr | letExpr | funExpr | dfunExpr | andExpr

  lazy val andExpr = and | orExpr
  lazy val and = orExpr ~ BAND ~ orExpr ^^ { case p1 ~ _ ~ p2 => BinOp(And, p1, p2)}

  lazy val orExpr = or | ltExpr
  lazy val or = ltExpr ~ BOR ~ ltExpr ^^ { case p1 ~ _ ~ p2 => BinOp(Or, p1, p2)}

  lazy val ltExpr = lessthan | pExpr
  lazy val lessthan = pExpr ~ LESS ~ pExpr ^^ { case p1 ~ _ ~ p2 => BinOp(Lt, p1, p2)}

  lazy val pExpr: Parser[Expr] = plus | minus | mExpr
  lazy val plus = mExpr ~ PLUS ~ mExpr ^^ { case p1 ~ _ ~ p2 => BinOp(Plus, p1, p2) }
  lazy val minus = mExpr ~ MINUS ~ mExpr ^^ { case p1 ~ _ ~ p2 => BinOp(Minus, p1, p2)}

  lazy val mExpr: Parser[Expr] = mult | appExpr
  lazy val mult = appExpr ~ MULT ~ appExpr ^^ { case p1 ~ _ ~ p2 => BinOp(Mult, p1, p2) }

  lazy val appExpr: Parser[Expr] = application | aExpr
  lazy val application = aExpr ~ rep1(aExpr) ^^ {
    case fun ~ args => args.tail.foldLeft(AppExp(fun, args.head)){ (acc, arg) => AppExp(acc, arg) }
  }

  lazy val aExpr = intv | truev | falsev | varv | paren

  lazy val name = not(reserved) ~> """[a-zA-Z][a-zA-Z0-9_]*""".r
  lazy val intv = """\d+""".r ^^ { case n => ILit(n.toInt) }
  lazy val truev = "true" ^^ { case _ => BLit(true) }
  lazy val falsev = "false" ^^ { case _ => BLit(false) }
  lazy val varv = not(reserved) ~> """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ { case s => Var(s) }
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

  lazy val dfunExpr: Parser[Expr] = (DFUN ~> rep1(name)) ~ (RARROW ~> expr) ^^ {
    case args ~ body => args.foldRight(body) { (arg, exp) => DFunExp(arg, exp) }
  }
}
