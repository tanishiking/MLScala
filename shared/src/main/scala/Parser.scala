package mlscala

import fastparse.WhitespaceApi
import mlscala.Ast._

object Parser {
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import White._

  def errorMessage[T](p: Parser[T], str: String) =
    ParseError(p.parse(str).asInstanceOf[Parsed.Failure]).getMessage

  private val IF = "if".!
  private val THEN = "then".!
  private val ELSE = "else".!
  private val LET = "let".!
  private val REC = "rec".!
  private val IN = "in".!
  private val FUN = "fun".!
  private val DFUN = "dfun".!
  private val AND = "and".!
  private val TRUE = "true".!
  private val FALSE = "false".!
  private val RARROW = "->".!
  private val SEMISEMI = ";;".!
  private val BAND = "&&".!
  private val BOR = "||".!
  private val EQUAL = "=".!
  private val LESS = "<".!
  private val PLUS = "+".!
  private val MINUS = "-".!
  private val MULT = "*".!
  private val LPAREN = "(".!
  private val RPAREN = ")".!
  private val reserved: P[String] = P(
    IF | THEN | ELSE | LET | REC | IN | FUN | DFUN | AND | TRUE | FALSE | RARROW | SEMISEMI | BAND | BOR | EQUAL | LESS | PLUS | MINUS | MULT | LPAREN | RPAREN)

  // TODO: prefixにreserved words が含まれている文字列もパースできるように
  private val ident: P[String]  = P(!reserved ~ CharsWhileIn('a' to 'z')).!
  private val variable: P[Expr] = P(ident.map(v => Var(v)))
  private val number: P[Expr]   = P(CharsWhileIn('0' to '9').!.map(_.toInt).map(v => ILit(v)))
  private val truev: P[Expr]    = P(TRUE).map(_ => BLit(true))
  private val falsev: P[Expr]   = P(FALSE).map(_ => BLit(false))

  private val ifExpr: P[Expr] = P(IF ~ expr ~ THEN ~ expr ~ ELSE ~ expr).map {
    case (_, cond, _, thenExpr, _, elseExpr) => IfExp(cond, thenExpr, elseExpr)
  }
  private val letExpr: P[Expr] = P(LET ~ ident ~ EQUAL ~ expr ~ IN ~ expr).map {
    case (_, identifier, _, e, _, body) => LetExp(identifier, e, body)
  }
  private val funExpr: P[Expr] = P(FUN ~ ident.rep(1) ~ RARROW ~ expr).map {
    case (_, args, _, body) => args.foldRight(body) { (arg, exp) => FunExp(arg, exp) }
  }
  private val letRecExpr: P[Expr] = P(LET ~ REC ~ ident ~ EQUAL ~ funExpr ~ IN ~ expr).map {
    case (_, _, identifier, _, func: FunExp, _, body) => LetRecExp(identifier, func, body)
  }
  private val dfunExpr: P[Expr] = P(DFUN ~ ident.rep(1) ~ RARROW ~ expr).map {
    case (_, args, _, body) => args.foldRight(body) { (arg, exp) => DFunExp(arg, exp) }
  }
  private val paren: P[Expr] = P(LPAREN ~ expr ~ RPAREN).map { case (_, e, _) => e }

  private val aExpr: P[Expr] = P(truev | falsev | variable | number | paren)

  private val application: P[Expr] = P(variable ~ aExpr.rep(1)).map {
    case (fun, args) => args.tail.foldLeft(AppExp(fun, args.head)) { (acc, arg) => AppExp(acc, arg) }
  }
  private val appExpr: P[Expr] = P(application | aExpr)

  private val mult: P[Expr] = P(appExpr ~ MULT ~ appExpr).map { case (p1, _, p2) => BinOp(Mult, p1, p2) }
  private val multExpr: P[Expr] = P(mult | appExpr)

  private val plus: P[Expr]  = P(multExpr ~ PLUS ~ multExpr).map { case (p1, _, p2) => BinOp(Plus, p1, p2) }
  private val minus: P[Expr] = P(multExpr ~ MINUS ~ multExpr).map { case (p1, _, p2) => BinOp(Minus, p1, p2)}
  private val arithmeticExpr: P[Expr] = P(plus | minus | multExpr)

  private val lessThan: P[Expr] = P(arithmeticExpr ~ LESS ~ arithmeticExpr).map { case (p1, _, p2) => BinOp(Lt, p1, p2)}
  private val ltExpr: P[Expr]   = P(lessThan | arithmeticExpr)

  private val or: P[Expr]     = P(ltExpr ~ BOR ~ ltExpr).map { case (p1, _, p2) => BinOp(Or, p1, p2)}
  private val orExpr: P[Expr] = P(or | ltExpr)

  private val and: P[Expr]     = P(orExpr ~ BAND ~ orExpr).map { case (p1, _, p2) => BinOp(And, p1, p2)}
  private val andExpr: P[Expr] = P(and | orExpr)

  private lazy val expr: P[Expr]  = P(ifExpr | letExpr | letRecExpr | funExpr | dfunExpr | andExpr)

  private val binding: P[Decl] = P(ident ~ EQUAL ~ expr).map { case (id, _, e) => Decl(id, e) }

  private val topExpr: P[Stmt] = P(expr ~ SEMISEMI).map { case (e, _) => TopExpr(e) }
  private val topLet: P[Stmt] = P(LET ~ binding.rep(min=1, sep=AND) ~ SEMISEMI).map {
    case (_, bindings, _) => MultiDecl(bindings.toList)
  }
  private val topDecl: P[Stmt] = P(LET ~ ident ~ ident.rep(1) ~ EQUAL ~ expr ~ SEMISEMI).map {
    case (_, id, args, _, body, _) =>
      MultiDecl(List(Decl(id, args.foldRight(body) { (arg, exp) => FunExp(arg, exp) })))
  }
  private val recDecl: P[Stmt] = P(LET ~ REC ~ ident ~ EQUAL ~ FUN ~ ident ~ RARROW ~ expr ~ SEMISEMI).map {
    case (_, _, id, _, _, arg, _, e, _) => RecDecl(id, arg, e)
  }
  val statement: P[Stmt] = P(topExpr | topLet | topDecl | recDecl)

  val program: P[Program] = {
    P(statement.rep(1)).map(res => res.toList)
  }
}
