package mlscala

import exceptions.VariableNotBoundException
import mlscala.Ast._
import mlscala.Environment._

object Eval {
  def applyPrim(op: BinaryOp, arg1: Expr, arg2: Expr): Either[Exception, Expr] = {
    (op, arg1, arg2) match {
      case (And, BLit(b1), BLit(b2))  => Right(BLit(b1 && b2))
      case (And, _, _)                => Left(new RuntimeException("Both arguments must be boolean: &&"))
      case (Or, BLit(b1), BLit(b2))   => Right(BLit(b1 || b2))
      case (Or, _, _)                 => Left(new RuntimeException("Both arguments must be boolean: ||"))
      case (Plus, ILit(i1), ILit(i2)) => Right(ILit(i1 + i2))
      case (Plus, _, _)               => Left(new RuntimeException("Both arguments must be integer: +"))
      case (Mult, ILit(i1), ILit(i2)) => Right(ILit(i1 * i2))
      case (Mult, _, _)               => Left(new RuntimeException("Both arguments must be integer: *"))
      case (Lt, ILit(i1), ILit(i2))   => Right(BLit(i1 < i2))
      case (Lt, _, _)                 => Left(new RuntimeException("Both arguments must be integer: <"))
    }
  }

  def evalExp(env: Env, expr: Expr): Either[Exception, Expr] = {
    expr match {
      case Var(x) =>
        lookup(Var(x), env) match {
          case Some(e) => Right(e)
          case None    => Left(new VariableNotBoundException("Variable " + x + " not bounded"))
        }
      case ILit(i) => Right(ILit(i))
      case BLit(b) => Right(BLit(b))
      case BinOp(op, e1, e2) => {
        for {
          // どちらかがLeftならそのLeftが返る
          arg1 <- evalExp(env, e1).right
          arg2 <- evalExp(env, e2).right
          res  <- applyPrim(op, arg1, arg2).right
        } yield res
      }
      case IfExp(e1, e2, e3) =>
        // evalExp(env, e1) が Left(_) な場合は Left(_) が返る
        evalExp(env, e1).right.flatMap {
          case BLit(true)  => evalExp(env, e2)
          case BLit(false) => evalExp(env, e3)
          case _           => Left(new RuntimeException("if expression must got boolean"))
        }
    }
  }

  def evalDecl(env: Env, prog: Program): Either[Exception, (String, Env, Expr)] = {
    prog match {
      case Exp(e)      => evalExp(env, e).right.flatMap(v => Right(("-", env, v)))
      case Decl(id, e) => evalExp(env, e).right.flatMap(v => Right((id, extendEnv(Var(id), e, env), v)))
    }
  }

}
