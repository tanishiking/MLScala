package mlscala

import exceptions.{VariableNotBoundException, TypeMismatchException}
import Ast._
import Environment.Env
import EvalResult._

object Eval {
  private def applyPrim(op: BinaryOp, arg1: EvalV, arg2: EvalV): Either[Exception, EvalV] = {
    (op, arg1, arg2) match {
      case (And, BoolV(b1), BoolV(b2)) => Right(BoolV(b1 && b2))
      case (Or, BoolV(b1), BoolV(b2))  => Right(BoolV(b1 || b2))
      case (Plus, IntV(i1), IntV(i2))  => Right(IntV(i1 + i2))
      case (Minus, IntV(i1), IntV(i2)) => Right(IntV(i1 - i2))
      case (Mult, IntV(i1), IntV(i2))  => Right(IntV(i1 * i2))
      case (Lt, IntV(i1), IntV(i2))    => Right(BoolV(i1 < i2))
      case _                           => Left(TypeMismatchException("both arguments must be same type"))
    }
  }

  private def evalExp(env: Env, expr: Expr): Either[Exception, EvalV] = {
    expr match {
      case Var(x) =>
        env.get(Var(x)) match {
          case Some(e) => Right(e)
          case None    => Left(new VariableNotBoundException("Variable " + x + " not bounded"))
        }
      case ILit(i) => Right(IntV(i))
      case BLit(b) => Right(BoolV(b))
      case BinOp(op, e1, e2) =>
        for {
          arg1 <- evalExp(env, e1).right
          arg2 <- evalExp(env, e2).right
          res  <- applyPrim(op, arg1, arg2).right
        } yield res
      case IfExp(e1, e2, e3) =>
        evalExp(env, e1).right.flatMap {
          case BoolV(true)  => evalExp(env, e2)
          case BoolV(false) => evalExp(env, e3)
          case _            => Left(new RuntimeException("if expression must got boolean"))
        }
      case LetExp(id, e, body) =>
        for {
          e1 <- evalExp(env, e).right
          e2 <- evalExp(env.updated(Var(id), e1), body).right
        } yield e2
      case FunExp(arg, body)  => Right(ProcV(arg, env, body))
      case DFunExp(arg, body) => Right(DProcV(arg, body))
      case AppExp(fun, arg)   =>
        (for {
          funval <- evalExp(env, fun).right
          argval <- evalExp(env, arg).right
        } yield (funval, argval)) match {
          case Right((ProcV(id, _env, body), arg: EvalV)) => evalExp(_env.updated(Var(id), arg), body)
          case Right((DProcV(id, body), arg: EvalV))      => evalExp(env.updated(Var(id), arg), body)
          case Right((PrintV(), arg: EvalV))              => println(arg); Right(arg)
          case Left(e: Exception)                         => Left(e)
          case _                                          => Left(new RuntimeException("Non-function value is applied"))
        }
    }
  }

  // Reduce List of Either to Eiler of list
  private def seqU[A, B](ls: Seq[Either[A, B]]): Either[A, Seq[B]] =
    ls.foldRight(Right(Nil): Either[A, List[B]]) {(l, acc) => for (xs <- acc.right; x <- l.right) yield x :: xs}

  def evalStmt(env: Env, stmt: Stmt): Either[Exception, EvalResult] = {
    stmt match {
      case TopExpr(e)       => evalExp(env, e).right.flatMap(v => Right(SingleEvalResult("-", env, v)))
      case MultiDecl(decls) => seqU(decls.map(d => evalExp(env, d.e))).right.flatMap(es =>
          Right(MultiEvalResult(
            decls.map(_.id),
            es.zip(decls.map(_.id)).foldLeft(env){ (curEnv, t: (EvalV, String)) => curEnv.updated(Var(t._2), t._1) },
            es)))
      case RecDecl(id, arg, body) =>
        val proc = DProcV(arg, body)
        Right(SingleEvalResult(id, env.updated(Var(id), proc), proc))
    }
  }
}
