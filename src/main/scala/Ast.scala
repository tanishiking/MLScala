package mlscala
import mlscala.Typing.TypeVariable

object Ast {
  sealed abstract class BinaryOp
  case object And extends BinaryOp
  case object Or extends BinaryOp
  case object Plus extends BinaryOp
  case object Minus extends BinaryOp
  case object Mult extends BinaryOp
  case object Lt extends BinaryOp

  sealed abstract class Expr
  case class Var(id: String) extends Expr
  case class ILit(v: Int) extends Expr
  case class BLit(v: Boolean) extends Expr
  case class BinOp(op: BinaryOp, e1: Expr, e2: Expr) extends Expr
  case class IfExp(cond: Expr, et: Expr, ef: Expr) extends Expr
  case class LetExp(id: String, e: Expr, body: Expr) extends Expr
  case class FunExp(arg: String, body: Expr) extends Expr
  case class DFunExp(arg: String, body: Expr) extends Expr
  case class AppExp(fun: Expr, arg: Expr) extends Expr

  sealed abstract class Program
  case class Exp(e: Expr) extends Program
  case class MultiDecl(bindings: List[Decl]) extends Program
  case class RecDecl(id: String, arg: String, exp: Expr) extends Program

  case class Decl(id: String, e: Expr)

  abstract sealed class Type
  case object TyInt extends Type
  case object TyBool extends Type
  case class TyVar(tyvar: TypeVariable) extends Type
  case class TyFun(ty1: Type, ty2: Type) extends Type

}
