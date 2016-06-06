import exceptions.{VariableNotBoundException, TypeMismatchException}
import org.scalatest.FlatSpec
import mlscala.Typing._
import mlscala.Ast._
import mlscala.Environment.getEmptyTyEnv

import scala.collection.immutable.ListMap


class TypeSpec extends FlatSpec {

  behavior of "Typing.scala"

  "BinaryOp PLUS MULT" should "return TyInt" in {
    assert(tyPrim(Plus, TyInt, TyInt).right.get === (List((TyInt, TyInt), (TyInt, TyInt)), TyInt))
    assert(tyPrim(Mult, TyInt, TyInt).right.get === (List((TyInt, TyInt), (TyInt, TyInt)), TyInt))
    assert(tyPrim(Minus, TyInt, TyInt).right.get === (List((TyInt, TyInt), (TyInt, TyInt)), TyInt))
  }

  "BinaryOp LT AND OR" should "return TyBool" in {
    assert(tyPrim(Lt, TyInt, TyInt).right.get === (List((TyInt, TyInt), (TyInt, TyInt)), TyBool))
    assert(tyPrim(And, TyBool, TyBool).right.get === (List((TyBool, TyBool), (TyBool, TyBool)), TyBool))
    assert(tyPrim(Or, TyBool, TyBool).right.get === (List((TyBool, TyBool), (TyBool, TyBool)), TyBool))
  }

  /*
  "BinaryOp typemismatch" should "raise TypeMismatchException" in {
    assert(tyPrim(Lt, TyInt, TyBool).left.get.isInstanceOf[TypeMismatchException])
    assert(tyPrim(And, TyInt, TyInt).left.get.isInstanceOf[TypeMismatchException])
    assert(tyPrim(Or, TyInt, TyInt).left.get.isInstanceOf[TypeMismatchException])
    assert(tyPrim(Plus, TyBool, TyInt).left.get.isInstanceOf[TypeMismatchException])
    assert(tyPrim(Mult, TyBool, TyBool).left.get.isInstanceOf[TypeMismatchException])
    assert(tyPrim(Minus, TyBool, TyBool).left.get.isInstanceOf[TypeMismatchException])
  }
  */

  "tyExp Var" should "return type of variable" in {
    assert(tyExp(Map(Var("x") -> TyInt), Var("x")).right.get === (getEmptySubsts, TyInt))
    assert(tyExp(Map(Var("x") -> TyBool), Var("x")).right.get === (getEmptySubsts, TyBool))
  }

  it should "raise variableNotBoundException" in {
    assert(tyExp(Map(Var("x") -> TyBool), Var("y")).left.get.isInstanceOf[VariableNotBoundException])
  }

  "tyExp Lit" should "return tyInt" in {
    assert(tyExp(getEmptyTyEnv, ILit(1)).right.get === (getEmptySubsts, TyInt))
  }

  it should "return tybool" in {
    assert(tyExp(getEmptyTyEnv, BLit(true)).right.get === (getEmptySubsts, TyBool))
  }

  "tyExp BinOp " should "same result of tyPrim BinaryOp" in {
    assert(tyExp(getEmptyTyEnv, BinOp(Plus, ILit(1), ILit(1))).right.get._2 === tyPrim(Plus, TyInt, TyInt).right.get._2)
  }

  "tyExp ifExp" should "return Ty" in {
    assert(tyExp(getEmptyTyEnv, IfExp(BLit(true), ILit(1), ILit(1))).right.get === (getEmptySubsts, TyInt))
    assert(tyExp(getEmptyTyEnv, IfExp(BLit(true), BLit(true), BLit(false))).right.get === (getEmptySubsts, TyBool))
  }

  /*
  it should "raise TypeMismatchException" in {
    assert(tyExp(getEmptyTyEnv, IfExp(ILit(1), ILit(1), ILit(1))).left.get.isInstanceOf[TypeMismatchException])
    assert(tyExp(getEmptyTyEnv, IfExp(BLit(true), BLit(true), ILit(1))).left.get.isInstanceOf[TypeMismatchException])
    assert(tyExp(getEmptyTyEnv, IfExp(BLit(true), BinOp(Plus, ILit(1), BLit(true)), ILit(1))).left.get.isInstanceOf[TypeMismatchException])
  }
  */

  "tyExp LetExp" should "return TyInt" in {
    assert(tyExp(getEmptyTyEnv, LetExp("x", ILit(1), Var("x"))).right.get === (getEmptySubsts, TyInt))
    assert(tyExp(Map(Var("x") -> TyInt), LetExp("y", ILit(1), Var("x"))).right.get === (getEmptySubsts, TyInt))
  }

  "substType" should "return tyint" in {
    assert(substType(ListMap.empty[TypeVariable, Type], TyInt) === TyInt)
  }

  it should "return tybool" in {
    assert(substType(ListMap.empty[TypeVariable, Type], TyBool) === TyBool)
  }

  it should "return Tyfun()" in {
    val alpha: TypeVariable = freshTyVar()
    val beta: TypeVariable = freshTyVar()
    assert(substType(ListMap(alpha -> TyInt), TyFun(TyVar(alpha), TyBool)) === TyFun(TyInt, TyBool))
    assert(substType(ListMap(beta -> TyFun(TyVar(alpha), TyInt), alpha -> TyBool), TyVar(beta)) === TyFun(TyBool, TyInt))
  }

  "unify" should "return unified substitutions" in {
    val alpha: TypeVariable = freshTyVar()
    val beta: TypeVariable = freshTyVar()
    assert(unify(Nil).right.get === ListMap.empty[TypeVariable, Type])
    assert(unify(List((TyVar(alpha), TyInt))).right.get === ListMap(alpha -> TyInt))
    assert(unify(List((TyFun(TyBool, TyVar(alpha)), TyFun(TyVar(beta), TyFun(TyInt, TyVar(beta)))))).right.get
      === ListMap(alpha -> TyFun(TyInt, TyBool), beta -> TyBool))
  }
}
