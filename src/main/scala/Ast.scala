package mlscala

object Ast {
  sealed abstract class BinaryOp
  case object And extends BinaryOp
  case object Or extends BinaryOp
  case object Plus extends BinaryOp
  case object Mult extends BinaryOp
  case object Lt extends BinaryOp

  sealed abstract class Expr
  case class Var(id: String) extends Expr
  case class ILit(v: Int) extends Expr
  case class BLit(v: Boolean) extends Expr
  case class BinOp(op: BinaryOp, e1: Expr, e2: Expr) extends Expr
  case class IfExp(cond: Expr, et: Expr, ef: Expr) extends Expr
  case class LetExp(id: String, e: Expr, body: Expr) extends Expr

  sealed abstract class Program
  case class Exp(e: Expr) extends Program
  case class MultiDecl(bindings: List[Decl]) extends Program

  case class Decl(id: String, e: Expr)

}
