package mlscala

import scala.util.parsing.combinator._

object Parser extends RegexParsers {

  def parse(input: String) = parseAll(toplevel, input)

  lazy val toplevel = expr <~ ";;"

  lazy val expr = ifExpr | ltExpr

  lazy val ltExpr = lessthan | pExpr
  lazy val lessthan = pExpr ~ "<" ~ pExpr ^^ { case p1 ~ _ ~ p2 => Ast.BinOp(Ast.Lt, p1, p2)}

  lazy val pExpr: Parser[Ast.Expr] = plus | mExpr
  lazy val plus = mExpr ~ "+" ~ mExpr ^^ { case p1 ~ _ ~ p2 => Ast.BinOp(Ast.Plus, p1, p2) }

  lazy val mExpr: Parser[Ast.Expr] = mult | aExpr
  lazy val mult = aExpr ~ "*" ~ aExpr ^^ { case p1 ~ _ ~ p2 => Ast.BinOp(Ast.Mult, p1, p2) }

  lazy val aExpr = intv | truev | falsev | varv | paren

  lazy val intv = """\d+""".r ^^ { case n => Ast.ILit(n.toInt) }
  lazy val truev = "true" ^^ { case _ => Ast.BLit(true) }
  lazy val falsev = "false" ^^ { case _ => Ast.BLit(false) }
  lazy val varv = """[a-zA-Z]+""".r ^^ { case s => Ast.Var(s) }
  lazy val paren = "(" ~> expr <~ ")"

  lazy val ifExpr: Parser[Ast.Expr] = ("if" ~> expr <~ "then") ~ expr ~ ("else" ~> expr) ^^ {
    case cond ~ et ~ ef => Ast.IfExp(cond, et, ef)
  }

}
