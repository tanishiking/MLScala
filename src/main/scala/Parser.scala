package mlscala

import mlscala.Ast._

import scala.util.parsing.combinator._

object Parser extends RegexParsers {

  def parse(input: String) = parseAll(toplevel, input)

  lazy val toplevel = expr <~ ";;" ^^ { case e => Exp(e) }

  lazy val expr = ifExpr | andExpr

  lazy val andExpr = and | orExpr
  lazy val and = orExpr ~ "&&" ~ orExpr ^^ { case p1 ~ _ ~ p2 => BinOp(And, p1, p2)}

  lazy val orExpr = or | ltExpr
  lazy val or = ltExpr ~ "||" ~ ltExpr ^^ { case p1 ~ _ ~ p2 => BinOp(Or, p1, p2)}

  lazy val ltExpr = lessthan | pExpr
  lazy val lessthan = pExpr ~ "<" ~ pExpr ^^ { case p1 ~ _ ~ p2 => BinOp(Lt, p1, p2)}

  lazy val pExpr: Parser[Expr] = plus | mExpr
  lazy val plus = mExpr ~ "+" ~ mExpr ^^ { case p1 ~ _ ~ p2 => BinOp(Plus, p1, p2) }

  lazy val mExpr: Parser[Expr] = mult | aExpr
  lazy val mult = aExpr ~ "*" ~ aExpr ^^ { case p1 ~ _ ~ p2 => BinOp(Mult, p1, p2) }

  lazy val aExpr = intv | truev | falsev | varv | paren

  lazy val intv = """\d+""".r ^^ { case n => ILit(n.toInt) }
  lazy val truev = "true" ^^ { case _ => BLit(true) }
  lazy val falsev = "false" ^^ { case _ => BLit(false) }
  lazy val varv = """[a-zA-Z]+""".r ^^ { case s => Var(s) }
  lazy val paren = "(" ~> expr <~ ")"

  lazy val ifExpr: Parser[Expr] = ("if" ~> expr <~ "then") ~ expr ~ ("else" ~> expr) ^^ {
    case cond ~ et ~ ef => IfExp(cond, et, ef)
  }

}
