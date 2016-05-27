import exceptions.{VariableNotBoundException, TypeMismatchException}
import org.scalatest.FlatSpec
import mlscala.Typing._
import mlscala.Ast._
import mlscala.Environment.getEmptyTyEnv


class TypeSpec extends FlatSpec {

  behavior of "Typing.scala"

  "BinaryOp PLUS MULT" should "return TyInt" in {
    assert(tyPrim(Plus, TyInt, TyInt).right.get === TyInt)
    assert(tyPrim(Mult, TyInt, TyInt).right.get === TyInt)
    assert(tyPrim(Minus, TyInt, TyInt).right.get === TyInt)
  }

  "BinaryOp LT AND OR" should "return TyBool" in {
    assert(tyPrim(Lt, TyInt, TyInt).right.get === TyBool)
    assert(tyPrim(And, TyBool, TyBool).right.get === TyBool)
    assert(tyPrim(Or, TyBool, TyBool).right.get === TyBool)
  }

  "BinaryOp typemismatch" should "raise TypeMismatchException" in {
    assert(tyPrim(Lt, TyInt, TyBool).left.get.isInstanceOf[TypeMismatchException])
    assert(tyPrim(And, TyInt, TyInt).left.get.isInstanceOf[TypeMismatchException])
    assert(tyPrim(Or, TyInt, TyInt).left.get.isInstanceOf[TypeMismatchException])
    assert(tyPrim(Plus, TyBool, TyInt).left.get.isInstanceOf[TypeMismatchException])
    assert(tyPrim(Mult, TyBool, TyBool).left.get.isInstanceOf[TypeMismatchException])
    assert(tyPrim(Minus, TyBool, TyBool).left.get.isInstanceOf[TypeMismatchException])
  }

  "tyExp Var" should "return type of variable" in {
    assert(tyExp(Map(Var("x") -> TyInt), Var("x")).right.get === TyInt)
    assert(tyExp(Map(Var("x") -> TyBool), Var("x")).right.get === TyBool)
  }

  it should "raise variableNotBoundException" in {
    assert(tyExp(Map(Var("x") -> TyBool), Var("y")).left.get.isInstanceOf[VariableNotBoundException])
  }

  "tyExp Lit" should "return tyInt" in {
    assert(tyExp(getEmptyTyEnv, ILit(1)).right.get === TyInt)
  }

  it should "return tybool" in {
    assert(tyExp(getEmptyTyEnv, BLit(true)).right.get === TyBool)
  }

  "tyExp BinOp " should "same result of tyPrim BinaryOp" in {
    assert(tyExp(getEmptyTyEnv, BinOp(Plus, ILit(1), ILit(1))).right.get === tyPrim(Plus, TyInt, TyInt).right.get)
  }

  "tyExp ifExp" should "return Tyint" in {
    assert(tyExp(getEmptyTyEnv, IfExp(BLit(true), ILit(1), ILit(1))).right.get === TyInt)
  }

  it should "return TyBool" in {
    assert(tyExp(getEmptyTyEnv, IfExp(BLit(true), BLit(true), BLit(false))).right.get === TyBool)
  }

  it should "raise TypeMismatchException" in {
    assert(tyExp(getEmptyTyEnv, IfExp(ILit(1), ILit(1), ILit(1))).left.get.isInstanceOf[TypeMismatchException])
    assert(tyExp(getEmptyTyEnv, IfExp(BLit(true), BLit(true), ILit(1))).left.get.isInstanceOf[TypeMismatchException])
    assert(tyExp(getEmptyTyEnv, IfExp(BLit(true), BinOp(Plus, ILit(1), BLit(true)), ILit(1))).left.get.isInstanceOf[TypeMismatchException])
  }

  "tyExp LetExp" should "return TyInt" in {
    assert(tyExp(getEmptyTyEnv, LetExp("x", ILit(1), Var("x"))).right.get === TyInt)
    assert(tyExp(Map(Var("x") -> TyInt), LetExp("y", ILit(1), Var("x"))).right.get === TyInt)
  }
}
