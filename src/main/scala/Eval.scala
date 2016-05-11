package mlscala

import exceptions.VariableNotBoundException
import mlscala.Ast._
import mlscala.Environment._
import mlscala.EvalResult._

object Eval {

  def applyPrim(op: BinaryOp, arg1: EvalV, arg2: EvalV): Either[Exception, EvalV] = {
    (op, arg1, arg2) match {
      case (And, BoolV(b1), BoolV(b2))  => Right(BoolV(b1 && b2))
      case (And, _, _)                => Left(new RuntimeException("Both arguments must be boolean: &&"))
      case (Or, BoolV(b1), BoolV(b2))   => Right(BoolV(b1 || b2))
      case (Or, _, _)                 => Left(new RuntimeException("Both arguments must be boolean: ||"))
      case (Plus, IntV(i1), IntV(i2)) => Right(IntV(i1 + i2))
      case (Plus, _, _)               => Left(new RuntimeException("Both arguments must be integer: +"))
      case (Minus, IntV(i1), IntV(i2)) => Right(IntV(i1 - i2))
      case (Minus, _, _)               => Left(new RuntimeException("Both arguments must be integer: -"))
      case (Mult, IntV(i1), IntV(i2)) => Right(IntV(i1 * i2))
      case (Mult, _, _)               => Left(new RuntimeException("Both arguments must be integer: *"))
      case (Lt, IntV(i1), IntV(i2))   => Right(BoolV(i1 < i2))
      case (Lt, _, _)                 => Left(new RuntimeException("Both arguments must be integer: <"))
    }
  }

  def evalExp(env: Env, expr: Expr): Either[Exception, EvalV] = {
    expr match {
      case Var(x) =>
        lookup(Var(x), env) match {
          case Some(e) => Right(e)
          case None    => Left(new VariableNotBoundException("Variable " + x + " not bounded"))
        }
      case ILit(i) => Right(IntV(i))
      case BLit(b) => Right(BoolV(b))
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
          case BoolV(true)  => evalExp(env, e2)
          case BoolV(false) => evalExp(env, e3)
          case _           => Left(new RuntimeException("if expression must got boolean"))
        }
      case LetExp(id, e, body) => {
        for {
          e1 <- evalExp(env, e).right
          e2 <- evalExp(extendEnv(Var(id), e1, env), body).right
        } yield e2
      }
      case FunExp(arg, body) => Right(ProcV(arg, env, body))
      case DFunExp(arg, body) => Right(DProcV(arg, body))
      case AppExp(fun, arg)  => {
        (for {
          funval <- evalExp(env, fun).right
          argval <- evalExp(env, arg).right
        } yield (funval, argval)) match {
          case Right((ProcV(id, _env, body), arg: EvalV)) => evalExp(extendEnv(Var(id), arg, _env), body)
          case Right((DProcV(id, body), arg: EvalV))      => evalExp(extendEnv(Var(id), arg, env), body)
          case Left(e: Exception)                         => Left(e)
          case _                                          => Left(new RuntimeException("Non-function value is applied"))
        }
      }
    }
  }

  // Reduce List of Either to Eiler of list
  private def listU[A, B](ls: List[Either[A, B]]): Either[A, List[B]] =
    ls.foldRight(Right(Nil): Either[A, List[B]]) {(l, acc) => for (xs <- acc.right; x <- l.right) yield x :: xs}

  def evalDecl(env: Env, prog: Program): Either[Exception, EvalResult] = {
    prog match {
      case Exp(e)           => evalExp(env, e).right.flatMap(v => Right(SingleEvalResult("-", env, v)))
      case MultiDecl(decls) => listU(decls.map(d => evalExp(env, d.e))).right.flatMap(es =>
          Right(MultiEvalResult(
            decls.map(_.id),
            es.zip(decls.map(_.id)).foldLeft(env){ (curEnv, t: (EvalV, String)) => extendEnv(Var(t._2), t._1, curEnv) },
            es)))
    }
  }

}
