package mlscala

object Ast {
  sealed abstract class BinaryOp
  case object And extends BinaryOp {
    override def toString: String = "&&"
  }
  case object Or extends BinaryOp {
    override def toString: String = "||"
  }
  case object Plus extends BinaryOp {
    override def toString: String = "+"
  }
  case object Minus extends BinaryOp {
    override def toString: String = "-"
  }
  case object Mult extends BinaryOp {
    override def toString: String = "*"
  }
  case object Lt extends BinaryOp {
    override def toString: String = "<"
  }

  sealed abstract class Expr
  case class Var(id: String) extends Expr
  case class ILit(v: Int) extends Expr
  case class BLit(v: Boolean) extends Expr
  case class BinOp(op: BinaryOp, e1: Expr, e2: Expr) extends Expr
  case class IfExp(cond: Expr, et: Expr, ef: Expr) extends Expr
  case class LetExp(id: String, e: Expr, body: Expr) extends Expr
  case class LetRecExp(id: String, func: FunExp, body: Expr) extends Expr
  case class FunExp(arg: String, body: Expr) extends Expr
  case class DFunExp(arg: String, body: Expr) extends Expr
  case class AppExp(fun: Expr, arg: Expr) extends Expr

  case class Decl(id: String, e: Expr)

  sealed abstract class Stmt
  case class TopExpr(e: Expr) extends Stmt
  case class MultiDecl(bindings: Seq[Decl]) extends Stmt
  case class RecDecl(id: String, arg: String, exp: Expr) extends Stmt

  type Program = Seq[Stmt]
}
