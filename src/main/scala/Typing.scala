package mlscala

import exceptions.{VariableNotBoundException, TypeMismatchException}
import mlscala.Ast._
import mlscala.Environment.TyEnv

object Typing {

  def tyPrim(op: BinaryOp, ty1: Type, ty2: Type): Either[Exception, Type] = {
    op match {
      case And => (ty1, ty2) match {
        case (TyBool, TyBool) => Right(TyBool)
        case _                => Left(new TypeMismatchException("Arguments must be of integer: &&"))
      }
      case Or => (ty1, ty2) match {
        case (TyBool, TyBool) => Right(TyBool)
        case _                => Left(new TypeMismatchException("Arguments must be of integer: ||"))
      }
      case Plus => (ty1, ty2) match {
        case (TyInt, TyInt) => Right(TyInt)
        case _              => Left(new TypeMismatchException("Arguments must be of integer: +"))
      }
      case Minus => (ty1, ty2) match {
        case (TyInt, TyInt) => Right(TyInt)
        case _              => Left(new TypeMismatchException("Arguments must be of integer: -"))
      }
      case Mult => (ty1, ty2) match {
        case (TyInt, TyInt) => Right(TyInt)
        case _              => Left(new TypeMismatchException("Arguments must be of integer: *"))
      }
      case Lt => (ty1, ty2) match {
        case (TyInt, TyInt) => Right(TyBool)
        case _              => Left(new TypeMismatchException("Arguments must be of integer: <"))
      }
    }
  }

  def tyExp(tyenv: TyEnv, e: Expr): Either[Exception, Type] = {
    e match {
      case Var(id) => tyenv.get(Var(id)) match {
        case Some(ty) => Right(ty)
        case None     => Left(new VariableNotBoundException("Variable not bound: " + id))
      }
      case ILit(_) => Right(TyInt)
      case BLit(_) => Right(TyBool)
      case BinOp(op, e1, e2) =>
        for {
          ty1 <- tyExp(tyenv, e1).right
          ty2 <- tyExp(tyenv, e2).right
          res <- tyPrim(op, ty1, ty2).right
        } yield res
      case IfExp(cond, e1, e2) =>
        (for {
          tycond <- tyExp(tyenv, cond).right
          ty1    <- tyExp(tyenv, e1).right
          ty2    <- tyExp(tyenv, e2).right
        } yield (tycond, ty1, ty2)) match {
          case Right((TyBool, TyInt, TyInt))   => Right(TyInt)
          case Right((TyBool, TyBool, TyBool)) => Right(TyBool)
          case Right((TyBool, _, _))           => Left(new TypeMismatchException("Type mismatch if"))
          case Right((TyInt, _, _))            => Left(new TypeMismatchException("Type mismatch if condition must be bool"))
          case Left(exception)                 => Left(exception)
        }
      case LetExp(id, e1, e2) =>
        for {
          ty1 <- tyExp(tyenv, e1).right
          ty2 <- tyExp(tyenv.updated(Var(id), ty1), e2).right
        } yield ty2
      case _ => Left(new TypeMismatchException("Not implemented!!!"))
    }
  }

  def tyDecl(tyenv: TyEnv, decl: Program): Either[Exception, Type] = {
    decl match {
      case Exp(e) => tyExp(tyenv, e)
      case _      => Left(new TypeMismatchException("Not implemeted!!!!"))
    }
  }

}
