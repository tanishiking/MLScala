package mlscala

import exceptions.{
  VariableNotBoundException,
  TypeMismatchException
}
import Ast._
import Type._

import scala.collection.immutable.ListMap

object Typer {
  case class TypeResult(substs: Substs, typ: Type)

  /**
    * 型環境に自由に出現しないという条件でスコープ外で宣言された変数の型として使われている
    * 型変数が多相性を持たないよう制限する
    * @param ty   : 型スキームへと変換する対象の型
    * @param tyenv: closure実行時点での型環境
    * @param subst: 型代入
    * @return     : ty を写像した型スキーム
    */
  private def closure(ty: Type, tyenv: TyEnv, subst: Substs): TyScheme = {
    def freeVarTyEnv(tyenv: TyEnv): TypeSet = {
      tyenv.foldLeft(Set.empty[TypeVariable])((acc, tup: (Var, TyScheme)) => acc ++ tup._2.freeVarTyScheme)
    }
    def freeVarialbes: TypeSet = freeVarTyEnv(tyenv).map { typevariable =>
      TyVar(typevariable).substitute(subst).freeVariables
    }.foldLeft(Set.empty[TypeVariable]) { (acc, typeSet) =>
      acc ++ typeSet
    }

    val polymorphicTypeVariables: TypeSet = ty.freeVariables -- freeVarialbes
    TyScheme(polymorphicTypeVariables.toList, ty)
  }

  private def unify(constraints: EquationSet): Either[Exception, Substs] = {
    def unifyTyVar(tyvar: TypeVariable, ty: Type, rest: EquationSet): Either[Exception, Substs] = {
      // tyvar が ty であるという型代入を残りの制約に適用して単一化を続行
      val sub = Substs(ListMap(tyvar -> ty))
      val eqs: EquationSet = rest.map(equation => (equation._1.substitute(sub), equation._2.substitute(sub)))
      unify(eqs).right.flatMap(unified => Right(Substs(sub.substs ++ unified.substs)))
    }
    constraints match {
      case Nil                                                             => Right(Substs.empty)
      case (TyInt, TyInt) :: rest                                          => unify(rest)
      case (TyBool, TyBool) :: rest                                        => unify(rest)
      case (TyFun(ta1, ta2), TyFun(tb1, tb2)) :: rest                      => unify((ta1, tb1) :: (ta2, tb2) :: rest)
      // TyVar(alpha) = TyVar(alpha) のような制約は含まれるべきでないし(含まれていた場合型代入の時点で無限ループになる)
      // TyVar(alpha) = TyFun(TyVar(alpha), TyInt) のような誤った制約は型付けの時点でおかしいので
      // !ty.freeVariables.contains(tyvar) が必要
      case (TyVar(tyvar), ty) :: rest if !ty.freeVariables.contains(tyvar) => unifyTyVar(tyvar, ty, rest)
      case (ty, TyVar(tyvar)) :: rest if !ty.freeVariables.contains(tyvar) => unifyTyVar(tyvar, ty, rest)
      case (ty1, ty2) :: _ if ty1 != ty2                                   => Left(TypeMismatchException(s"TypeError: expected $ty2, actual: $ty1"))
      case (ty1, ty2) :: _                                                 => Left(TypeMismatchException(s"TypeError: $ty1 and $ty2 must not be same type"))
    }
  }

  private def typeBinOp(op: BinaryOp, ty1: Type, ty2: Type): Either[Exception, TypeResult] = {
    def genSubsts(bothSideType: Type): Substs = {
      Substs(
        (ty1.freeVariables ++ ty2.freeVariables).map { tyvar =>
          tyvar -> bothSideType
        }(collection.breakOut)
      )
    }
    if (ty1.isInstanceOf[TyFun] || ty2.isInstanceOf[TyFun]) {
      Left(TypeMismatchException(s"TypeError: Type $ty1 $op $ty2 is invalid"))
    } else {
      op match {
        case And   => Right(TypeResult(genSubsts(TyBool), TyBool))
        case Or    => Right(TypeResult(genSubsts(TyBool), TyBool))
        case Plus  => Right(TypeResult(genSubsts(TyInt), TyInt))
        case Minus => Right(TypeResult(genSubsts(TyInt), TyInt))
        case Mult  => Right(TypeResult(genSubsts(TyInt), TyInt))
        case Lt    => Right(TypeResult(genSubsts(TyInt), TyBool))
      }
    }
  }

  private def typeExpr(tyenv: TyEnv, e: Expr): Either[Exception, TypeResult] = {
    e match {
      case Var(id) => tyenv.get(Var(id)) match {
        case Some(typeScheme) =>
          // val s: Substs = typeScheme.tyvars.foldLeft(Substs.empty) { (acc, tyvar) =>
          //   Substs(acc.substs.updated(tyvar, TyVar.fresh))
          // }
          Right(TypeResult(Substs.empty, typeScheme.ty))
          // Right(TypeResult(Substs.empty, typeScheme.ty.substitute(s)))
        case None     => Left(new VariableNotBoundException("Variable not bound: " + id))
      }
      case ILit(_) => Right(TypeResult(Substs.empty, TyInt))
      case BLit(_) => Right(TypeResult(Substs.empty, TyBool))
      case BinOp(op, e1, e2) =>
        for {
          res1   <- typeExpr(tyenv, e1).right
          res2   <- typeExpr(tyenv, e2).right
          res3   <- typeBinOp(op, res1.typ, res2.typ).right
          substs <- unify(
            (res1.typ, res2.typ) :: res1.substs.toEquationSet ++ res2.substs.toEquationSet ++ res3.substs.toEquationSet
          ).right
        } yield TypeResult(substs, res3.typ.substitute(substs))
      case IfExp(cond, e1, e2) =>
        (for {
          typeCond <- typeExpr(tyenv, cond).right
          typeThen <- typeExpr(tyenv, e1).right
          typeElse <- typeExpr(tyenv, e2).right
          substs   <- unify(
            List((typeThen.typ, typeElse.typ)) ++
            typeCond.substs.toEquationSet ++
            typeThen.substs.toEquationSet ++
            typeElse.substs.toEquationSet
          ).right
        } yield (typeCond.typ, typeThen.typ, substs)) match {
          case Right((TyBool, ty, substs)) => Right(TypeResult(substs, ty))
          case Right((tycond, _, _))       => Left(TypeMismatchException(s"TypeError: expected: $TyBool, actual: $tycond"))
          case Left(exception)             => Left(exception)
        }
      case LetExp(id, e1, e2) =>
        // letのスコープ外で定義された変数の型(変数)は、その変数がスコープの外でどのように
        // 使われるかに依存して型が決定するため、それらの型変数に多層性をもたせては実行時に型エラーが発生する可能性がある。
        // 例えば let f x = ((let g y = (x, y) in g 4), x + 1)
        // let g y = ... の型推論の時点で x を任意の型を取れるように束縛してしまうと本来
        // x + 1 の(単一化の)時点で x は integer だと推論されるはずなのに、x = true の場合も型チェックを通してしまい
        // true + 1 の時点でエラーになってしまう。
        // これを防ぐために closure により、この時点で tyenv に自由に出現する変数を除いた型変数のみを任意の型をとれるように
        // body の型スキームを作る.
        for {
          typeLet  <- typeExpr(tyenv, e1).right
          typeBody <- typeExpr(tyenv.updated(Var(id), closure(typeLet.typ, tyenv, typeLet.substs)), e2).right
          substs   <- unify(typeLet.substs.toEquationSet ++ typeBody.substs.toEquationSet).right
        } yield TypeResult(substs, typeBody.typ.substitute(substs))
      case LetRecExp(id, func, body) =>
        val domainType = TyVar.fresh
        val rangeType = TyVar.fresh
        for {
          typeFunc <- typeExpr(
            tyenv.updated(Var(id), TyFun(domainType, rangeType).typeScheme).updated(Var(func.arg), domainType.typeScheme),
            func
          ).right
          typeBody <- typeExpr(tyenv.updated(Var(id), closure(typeFunc.typ, tyenv, typeFunc.substs)), body).right
          substs <- unify((TyFun(domainType, rangeType), typeFunc.typ) :: typeFunc.substs.toEquationSet ++ typeBody.substs.toEquationSet).right
        } yield TypeResult(substs, typeBody.typ.substitute(substs))
      case FunExp(arg, body) =>
        val domainType: Type = TyVar.fresh
        typeExpr(tyenv.updated(Var(arg), domainType.typeScheme), body).right.flatMap { case TypeResult(sub, rangeType) =>
          Right(TypeResult(sub, TyFun(domainType.substitute(sub), rangeType)))
        }
      case AppExp(fun, arg) =>
        val rangeType: Type = TyVar.fresh
        for {
          typeFun <- typeExpr(tyenv, fun).right
          typeArg <- typeExpr(tyenv, arg).right
          substs  <- unify(
            List((TyFun(typeArg.typ, rangeType), typeFun.typ)) ++
            typeFun.substs.toEquationSet ++
            typeArg.substs.toEquationSet
          ).right
        } yield TypeResult(substs, rangeType.substitute(substs))
      case _ => Left(TypeMismatchException("Not implemented!!!"))
    }
  }

  private def seqU[A, B](ls: Seq[Either[A, B]]): Either[A, Seq[B]] =
    ls.foldRight(Right(Nil): Either[A, List[B]]) {(l, acc) => for (xs <- acc.right; x <- l.right) yield x :: xs}

  def typeStmt(tyenv: TyEnv, stmt: Stmt): Either[Exception, (TyEnv, Type)] = {
    stmt match {
      case TopExpr(e) => typeExpr(tyenv, e).right.map { case TypeResult(substs, ty) => (tyenv, ty) }
      case MultiDecl(decls) => seqU(decls.map(d => typeExpr(tyenv, d.e))).right.flatMap { typeResults =>
        Right((
          typeResults.zip(decls.map(_.id)).foldLeft(tyenv) { (curEnv: TyEnv, t: (TypeResult, String)) =>
            curEnv.updated(Var(t._2), t._1.typ.typeScheme)
          },
          typeResults.last.typ
        ))
      }
      case RecDecl(id, arg, body) =>
        val domainType = TyVar.fresh
        val rangeType = TyVar.fresh
        (for {
          typeBody <- typeExpr(
            tyenv.updated(Var(id), TyFun(domainType, rangeType).typeScheme).updated(Var(arg), domainType.typeScheme),
            body
          ).right
          substs <- unify(typeBody.substs.toEquationSet).right
        } yield TypeResult(substs, TyFun(domainType.substitute(substs), rangeType.substitute(substs)))) match {
          case Right(typeResult) => Right(tyenv.updated(Var(id), typeResult.typ.typeScheme), typeResult.typ)
          case Left(exception)   => Left(exception)
        }
      case _ => Left(TypeMismatchException("Not implemeted!!!!"))
    }
  }
}
