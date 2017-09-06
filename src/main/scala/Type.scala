package mlscala

import scala.collection.immutable.ListMap
import Ast.Var

object Type {
  type TypeVariable = Int
  type TypeSet = Set[TypeVariable]
  type EquationSet = Seq[(Type, Type)]

  type TyEnv = Map[Var, TyScheme] // let多相のため型環境は変数と型スキームとの対応とする
  object TyEnv {
    val initialEnv: TyEnv = Map(Var("print") -> TyScheme(List(0), TyVar(0)))
    val empty: TyEnv = Map.empty[Var, TyScheme]
  }

  case class Substs(substs: ListMap[TypeVariable, Type]) {
    def toEquationSet: EquationSet = substs.map{
      case (tyvar, ty) => (TyVar(tyvar), ty)
    }.toSeq
  }
  object Substs {
    def empty: Substs = Substs(ListMap.empty[TypeVariable, Type])
  }

  private var count: TypeVariable = 1
  private def freshTyVar(): TypeVariable = {
    count += 1
    count - 1
  }

  sealed trait Type {
    override def toString: String = "-"
    def freeVariables: TypeSet
    def substitute(substs: Substs): Type
    def typeScheme: TyScheme = TyScheme(this.freeVariables.toList, this)
  }
  case object TyInt extends Type {
    override def toString: String = "int"
    override def freeVariables: TypeSet = Set.empty[TypeVariable]
    override def substitute(substs: Substs): Type = TyInt
  }
  case object TyBool extends Type {
    override def toString: String = "bool"
    override def freeVariables: TypeSet = Set.empty[TypeVariable]
    override def substitute(substs: Substs): Type = TyBool
  }
  case class TyVar(tyvar: TypeVariable) extends Type { // monomorphic type variable
    override def freeVariables: TypeSet = Set(tyvar)
    override def substitute(substs: Substs): Type =
      substs.substs.get(tyvar) match {
        case Some(typ) => typ.substitute(substs)
        case None      => TyVar(tyvar)
    }
  }
  object TyVar {
    val fresh: TyVar = TyVar(freshTyVar())
  }
  case class TyFun(ty1: Type, ty2: Type) extends Type {
    override def toString: String = s"$ty1 -> $ty2"
    override def freeVariables: TypeSet = ty1.freeVariables ++ ty2.freeVariables
    override def substitute(substs: Substs): Type = TyFun(ty1.substitute(substs), ty2.substitute(substs))
  }

  // polymorphic type variable
  // 有限個の型変数により束縛された型
  // いわゆる有限個の型を受け取って型を返す関数のようなもの
  case class TyScheme(tyvars: List[TypeVariable], ty: Type) {
    def freeVarTyScheme: TypeSet = {
      tyvars.toSet ++ ty.freeVariables
    }
  }
}
