package mlscala

import exceptions.TypeMismatchException
import org.scalatest.{FunSpec, Matchers}
import Typer.typeStmt
import Type._
import Ast._


class TypeSpec extends FunSpec with Matchers {
  describe("Typing") {
    describe("typeExpr") {
      describe("TyVar") {
        it ("should type variable from ty env") {
          typeStmt(Map(Var("x") -> TyInt.typeScheme), TopExpr(Var("x"))).right.get shouldBe(Map(Var("x") -> TyInt.typeScheme), TyInt)
        }
      }

      describe("TyInt") {
        it ("should type integer") {
          typeStmt(TyEnv.empty, TopExpr(ILit(0))).right.get shouldBe (TyEnv.empty, TyInt)
        }
      }

      describe("TyBool") {
        it ("should type boolean") {
          typeStmt(TyEnv.empty, TopExpr(BLit(true))).right.get shouldBe (TyEnv.empty, TyBool)
        }
      }

      describe("BinOp") {
        it ("should type binary operation") {
          typeStmt(TyEnv.empty, TopExpr(BinOp(And, BLit(true), BLit(true)))).right.get shouldBe (TyEnv.empty, TyBool)
          typeStmt(TyEnv.empty, TopExpr(BinOp(Or, BLit(true), BLit(true)))).right.get shouldBe (TyEnv.empty, TyBool)
          typeStmt(TyEnv.empty, TopExpr(BinOp(Plus, ILit(0), ILit(0)))).right.get shouldBe (TyEnv.empty, TyInt)
          typeStmt(TyEnv.empty, TopExpr(BinOp(Minus, ILit(0), ILit(0)))).right.get shouldBe (TyEnv.empty, TyInt)
          typeStmt(TyEnv.empty, TopExpr(BinOp(Mult, ILit(0), ILit(0)))).right.get shouldBe (TyEnv.empty, TyInt)
          typeStmt(TyEnv.empty, TopExpr(BinOp(Lt, ILit(0), ILit(0)))).right.get shouldBe (TyEnv.empty, TyBool)
        }

        it ("should type mismatch with invalid binary operation") {
          typeStmt(TyEnv.empty, TopExpr(BinOp(And, ILit(1), BLit(true)))).left.get.isInstanceOf[TypeMismatchException] shouldBe true
          typeStmt(TyEnv.empty, TopExpr(BinOp(Or, ILit(1), BLit(true)))).left.get.isInstanceOf[TypeMismatchException] shouldBe true
        }
      }

      describe("IfExpr") {
        it ("should type if expression return then exp's (and else's) type") {
          typeStmt(TyEnv.empty, TopExpr(IfExp(BLit(true), ILit(1), ILit(1)))).right.get shouldBe (TyEnv.empty, TyInt)
          typeStmt(TyEnv.empty, TopExpr(IfExp(BinOp(Lt, ILit(1), ILit(1)), ILit(1), ILit(1)))).right.get shouldBe (TyEnv.empty, TyInt)
          typeStmt(TyEnv.empty, TopExpr(IfExp(BLit(true), BLit(false), BLit(true)))).right.get shouldBe (TyEnv.empty, TyBool)
        }

        it ("should fail typing if then expr and else expr have different type") {
          typeStmt(TyEnv.empty, TopExpr(IfExp(BLit(true), ILit(1), BLit(true)))).left.get.isInstanceOf[TypeMismatchException] shouldBe true
        }

        it ("should fail typing if conditional expr has not boolean type") {
          typeStmt(TyEnv.empty, TopExpr(IfExp(ILit(1), ILit(1), ILit(1)))).left.get.isInstanceOf[TypeMismatchException] shouldBe true
          typeStmt(TyEnv.empty, TopExpr(IfExp(BinOp(Plus, ILit(1), ILit(1)), ILit(1), ILit(1)))).left.get.isInstanceOf[TypeMismatchException] shouldBe true
        }
      }

      describe("letExpr") {
        it ("should type let expression") {
          typeStmt(TyEnv.empty, TopExpr(LetExp("x", ILit(1), Var("x")))).right.get shouldBe (TyEnv.empty, TyInt)
          typeStmt(TyEnv.empty, TopExpr(LetExp("x", BLit(true), Var("x")))).right.get shouldBe (TyEnv.empty, TyBool)
          typeStmt(TyEnv.empty, TopExpr(LetExp("id", FunExp("x", Var("x")), AppExp(Var("id"), ILit(1))))).right.get shouldBe (TyEnv.empty, TyInt)
        }
      }

      describe("funExpr") {
        it ("should type fun exp") {
          typeStmt(TyEnv.empty, TopExpr(FunExp("x", Var("x")))).right.get._2 match {
            case TyFun(ty1, ty2) if ty1.isInstanceOf[TyVar] && ty2.isInstanceOf[TyVar] => ty1 shouldBe ty2
            case _ => fail()
          }
        }
      }

      describe("appExpr") {
        it ("should type application") {
          val freshTyVar = TyVar.fresh
          typeStmt(
            Map(Var("id") -> TyFun(freshTyVar, freshTyVar).typeScheme),
            TopExpr(AppExp(Var("id"), ILit(3)))
          ).right.get shouldBe
            (
              Map(Var("id") -> TyFun(freshTyVar, freshTyVar).typeScheme),
              TyInt
            )
        }
      }
    }
  }
}
