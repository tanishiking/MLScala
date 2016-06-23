package mlscala

import exceptions.{VariableNotBoundException, TypeMismatchException}
import mlscala.Ast._
import mlscala.Environment.TyEnv

import scala.collection.immutable.ListMap

object Typing {

  type TypeVariable = Int
  type TySet = Set[TypeVariable]
  type Substs = ListMap[TypeVariable, Type]
  def getEmptySubsts: Substs = ListMap.empty[TypeVariable, Type]
  type TypePairs = List[(Type, Type)]
  private var count: TypeVariable = 0

  def tyschemeOfType(ty: Type): Tysc = TyScheme(Nil, ty)

  def freeVarTyScheme(tyscheme: Tysc): TySet = {
    tyscheme match {
      case TyScheme(tyvars, ty) => tyvars.toSet ++ freeVarTy(ty)
    }
  }

  def freeVarTyEnv(tyenv: TyEnv): TySet = {
    tyenv.foldLeft(Set.empty[TypeVariable])((acc, tup: (Var, Tysc)) => acc ++ freeVarTyScheme(tup._2))
  }

  def closure(ty: Type, tyenv: TyEnv, subst: Substs): Tysc = {
    def freeVarialbes: TySet = freeVarTyEnv(tyenv).map(typevariable => freeVarTy(substType(subst, TyVar(typevariable))))
      .foldLeft(Set.empty[TypeVariable])((acc, tyset) => acc ++ tyset)
    val ids: TySet = freeVarTy(ty) -- freeVarialbes
    TyScheme(ids.toList, ty)
  }

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

  def eqsOfSubst(substs: Substs): TypePairs = substs.map{ case (tyvar, ty) => (TyVar(tyvar), ty) }.toList

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

  def tyPrim(op: BinaryOp, ty1: Type, ty2: Type): Either[Exception, (TypePairs, Type)] = {
    // TODO: ty1 != ty2 のとき unify で typemismatch が起きるが...
    op match {
        case And   => Right(List((ty1, TyBool), (ty2, TyBool)), TyBool)
        case Or    => Right(List((ty1, TyBool), (ty2, TyBool)), TyBool)
        case Plus  => Right(List((ty1, TyInt), (ty2, TyInt)), TyInt)
        case Minus => Right(List((ty1, TyInt), (ty2, TyInt)), TyInt)
        case Mult  => Right(List((ty1, TyInt), (ty2, TyInt)), TyInt)
        case Lt    => Right(List((ty1, TyInt), (ty2, TyInt)), TyBool)
    }
  }

  def tyExp(tyenv: TyEnv, e: Expr): Either[Exception, (Substs, Type)] = {
    e match {
      case Var(id) => tyenv.get(Var(id)) match {
        case Some(tysc) => tysc match {
          case TyScheme(vars, ty) =>
            val s: Substs = vars.foldLeft(getEmptySubsts)((acc, id) => acc.updated(id, TyVar(freshTyVar())))
            Right((getEmptySubsts, substType(s, ty)))
        }
        case None     => Left(new VariableNotBoundException("Variable not bound: " + id))
      }
      case ILit(_) => Right(getEmptySubsts, TyInt)
      case BLit(_) => Right(getEmptySubsts, TyBool)
      case BinOp(op, e1, e2) =>
        for {
          // TODO: scalaz.\/ 使ったほうがすっきりしそう
          subty1 <- tyExp(tyenv, e1).right
          subty2 <- tyExp(tyenv, e2).right
          eqsty  <- tyPrim(op, subty1._2, subty2._2).right
          substs <- unify(eqsOfSubst(subty1._1) ++ eqsOfSubst(subty2._1) ++ eqsty._1).right
        } yield (substs, substType(substs, eqsty._2))
      case IfExp(cond, e1, e2) =>
        (for {
          // TODO: subty2._2 != subty3._2 のとき TypeMismatchException を raise するように
          subty1 <- tyExp(tyenv, cond).right
          subty2 <- tyExp(tyenv, e1).right
          subty3 <- tyExp(tyenv, e2).right
          substs <- unify((subty2._2, subty3._2) :: eqsOfSubst(subty1._1) ++ eqsOfSubst(subty2._1) ++ eqsOfSubst(subty3._1)).right
        } yield (subty1._2, subty2._2, substs)) match {
          case Right((TyBool, ty, substs))  => Right(substs, ty)
          case Right((_, _, _))             => Left(new TypeMismatchException("Type mismatch if condition must be bool"))
          case Left(exception)              => Left(exception)
        }
      case LetExp(id, e1, e2) =>
        for {
          subty1 <- tyExp(tyenv, e1).right
          subty2 <- tyExp(tyenv.updated(Var(id), closure(subty1._2, tyenv, subty1._1)), e2).right
          substs <- unify(eqsOfSubst(subty1._1) ++ eqsOfSubst(subty2._1)).right
        } yield (substs, substType(substs, subty2._2))
      case FunExp(arg, body) =>
        val domTy: Type = TyVar(freshTyVar())
        tyExp(tyenv.updated(Var(arg), tyschemeOfType(domTy)), body).right.flatMap { case (sub, ranTy) =>
          Right((sub, TyFun(substType(sub, domTy), ranTy)))
        }
      case AppExp(fun, arg) =>
        val ranTy: Type = TyVar(freshTyVar())
        for {
          subty1 <- tyExp(tyenv, fun).right
          subty2 <- tyExp(tyenv, arg).right
          substs <- unify((subty1._2, TyFun(subty2._2, ranTy)) :: eqsOfSubst(subty1._1) ++ eqsOfSubst(subty2._1)).right
        } yield (substs, substType(substs, ranTy))
      case _ => Left(new TypeMismatchException("Not implemented!!!"))
    }
  }

  def tyDecl(tyenv: TyEnv, decl: Program): Either[Exception, (TyEnv, Type)] = {
    decl match {
      case Exp(e) => tyExp(tyenv, e).right.map { case (substs, ty) => (tyenv, ty) }
      case _      => Left(new TypeMismatchException("Not implemeted!!!!"))
    }
  }

}
