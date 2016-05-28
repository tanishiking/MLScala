package mlscala

import exceptions.{VariableNotBoundException, TypeMismatchException}
import mlscala.Ast._
import mlscala.Environment.TyEnv

import scala.collection.immutable.ListMap

object Typing {

  type TypeVariable = Int
  type TySet = Set[TypeVariable]
  type Substs = ListMap[TypeVariable, Type]
  type TypePairs = List[(Type, Type)]
  private var count: TypeVariable = 0
  def freshTyVar(): TypeVariable = {
    count += 1
    count - 1
  }

  private def freeVarTy(ty: Type): TySet = {
    ty match {
      case TyInt           => Set.empty[TypeVariable]
      case TyBool          => Set.empty[TypeVariable]
      case TyVar(tyvar)    => Set(tyvar)
      case TyFun(ty1, ty2) => freeVarTy(ty1) ++ freeVarTy(ty2)
    }
  }

  private def getPretyTy(ty: Type): String = {
    ty match {
      case TyInt       => "int"
      case TyBool      => "boolean"
      case TyVar(_)    => "tyvar: "
      case TyFun(_, _) => "TyFun: "
    }
  }

  def substType(subst: Substs, ty: Type): Type = {
    ty match {
      case TyInt           => TyInt
      case TyBool          => TyBool
      case TyFun(ty1, ty2) => TyFun(substType(subst, ty1), substType(subst, ty2))
      case TyVar(tyvar)    =>
        subst.get(tyvar) match {
          case Some(typ) => substType(subst, typ)
          case None      => TyVar(tyvar)
        }
    }
  }

  def unify(tpair: TypePairs): Either[Exception, Substs] = {
    def unifyTyVar(tyvar: TypeVariable, ty: Type, rest: TypePairs): Either[Exception, Substs] = {
      val sub: Substs = ListMap(tyvar -> ty)
      val eqs: TypePairs = rest.map(pair => (substType(sub, pair._1), substType(sub, pair._2)))
      unify(eqs).right.flatMap(substs => Right(sub ++ substs))
    }
    tpair match {
      case Nil                                                          => Right(ListMap.empty[TypeVariable, Type])
      case (TyInt, TyInt) :: rest                                       => unify(rest)
      case (TyBool, TyBool) :: rest                                     => unify(rest)
      case (TyFun(ta1, ta2), TyFun(tb1, tb2)) :: rest                   => unify((ta1, tb1) :: (ta2, tb2) :: rest)
      case (TyVar(tyvar), ty) :: rest if !freeVarTy(ty).contains(tyvar) => unifyTyVar(tyvar, ty, rest)
      case (ty, TyVar(tyvar)) :: rest if !freeVarTy(ty).contains(tyvar) => unifyTyVar(tyvar, ty, rest)
      case _                                                            => Left(new RuntimeException("Unification error"))
    }
  }

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
