package mlscala

import mlscala.Ast._
import mlscala.Environment._

object Eval {
  def applyPrim(op: BinaryOp, arg1: Expr, arg2: Expr) = {
    (op, arg1, arg2) match {
      case (Plus, ILit(i1), ILit(i2)) => ILit(i1 + i2)
      case (Plus, _, _)               => sys.error("Both arguments must be integer: +")
      case (Mult, ILit(i1), ILit(i2)) => ILit(i1 * i2)
      case (Mult, _, _)               => sys.error("Both arguments must be integer: *")
      case (Lt, ILit(i1), ILit(i2))   => BLit(i1 < i2)
      case (Lt, _, _)                 => sys.error("Both arguments must be integer: <")
    }
  }

  def evalExp(env: Env, expr: Expr): Expr = {
    expr match {
      case Var(x) =>
        lookup(Var(x), env) match {
          case Some(e) => e
          case None    => sys.error("variable not bound: Var " + x)
        }
      case ILit(i) => ILit(i)
      case BLit(b) => BLit(b)
      case BinOp(op, e1, e2) => {
        val arg1 = evalExp(env, e1)
        val arg2 = evalExp(env, e2)
        applyPrim(op, arg1, arg2)
      }
      case IfExp(e1, e2, e3) =>
        evalExp(env, e1) match {
          case BLit(true)  => evalExp(env, e2)
          case BLit(false) => evalExp(env, e3)
          case _           => sys.error("if must be boolean")
        }
    }
  }

  def evalDecl(env: Env, prog: Program) = {
    prog match {
      case Exp(e) => ("-", env, evalExp(env, e))
    }
  }

}
