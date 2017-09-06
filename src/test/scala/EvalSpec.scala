package mlscala

import Ast._
import Environment._
import EvalResult._
import org.scalatest.{Matchers, FunSpec}

class EvalSpec extends FunSpec with Matchers {

  describe("Eval") {
    describe(".applyPrim") {
      it ("should return Either.Right for valid input") {
        Eval.applyPrim(Plus, IntV(1), IntV(3)) shouldBe Right(IntV(4))
        Eval.applyPrim(Minus, IntV(3), IntV(1)) shouldBe Right(IntV(2))
        Eval.applyPrim(Minus, IntV(3), IntV(5)) shouldBe Right(IntV(-2))
        Eval.applyPrim(Mult, IntV(3), IntV(3)) shouldBe Right(IntV(9))
        Eval.applyPrim(And, BoolV(true), BoolV(true)) shouldBe Right(BoolV(true))
        Eval.applyPrim(And, BoolV(false), BoolV(true)) shouldBe Right(BoolV(false))
        Eval.applyPrim(And, BoolV(true), BoolV(false)) shouldBe Right(BoolV(false))
        Eval.applyPrim(And, BoolV(false), BoolV(false)) shouldBe Right(BoolV(false))
        Eval.applyPrim(Or, BoolV(true), BoolV(true)) shouldBe Right(BoolV(true))
        Eval.applyPrim(Or, BoolV(false), BoolV(true)) shouldBe Right(BoolV(true))
        Eval.applyPrim(Or, BoolV(true), BoolV(false)) shouldBe Right(BoolV(true))
        Eval.applyPrim(Or, BoolV(false), BoolV(false)) shouldBe Right(BoolV(false))
        Eval.applyPrim(Lt, IntV(1), IntV(5)) shouldBe Right(BoolV(true))
        Eval.applyPrim(Lt, IntV(5), IntV(1)) shouldBe Right(BoolV(false))
        Eval.applyPrim(Lt, IntV(5), IntV(5)) shouldBe Right(BoolV(false))
      }

      it ("should return Either.Left for invalid input") {
        Eval.applyPrim(And, IntV(1), BoolV(true)).left.get.isInstanceOf[RuntimeException] shouldBe true
        Eval.applyPrim(Or, IntV(1), BoolV(true)).left.get.isInstanceOf[RuntimeException] shouldBe true
        Eval.applyPrim(Plus, BoolV(true), IntV(1)).left.get.isInstanceOf[RuntimeException] shouldBe true
        Eval.applyPrim(Minus, BoolV(true), IntV(1)).left.get.isInstanceOf[RuntimeException] shouldBe true
        Eval.applyPrim(Mult, BoolV(true), IntV(2)).left.get.isInstanceOf[RuntimeException] shouldBe true
        Eval.applyPrim(Lt, BoolV(true), IntV(2)).left.get.isInstanceOf[RuntimeException] shouldBe true
      }
    }
  }

  /*

  it should "properly eval exp" in {
    assert(evalExp(extendEnv(Var("x"), IntV(3), getEmptyEnv), BinOp(Plus, Var("x"), Var("x"))) === Right(IntV(6)))
    assert(evalExp(getEmptyEnv, LetExp("x", ILit(1), Var("x"))) === Right(IntV(1)))
    assert(evalExp(Map(Var("addx") -> ProcV("x", Map(), BinOp(Plus, Var("x"), ILit(1)))), AppExp(Var("addx"), ILit(1))).right.get === IntV(2))
    assert(evalExp(Map(Var("addx") -> DProcV("x", BinOp(Plus, Var("x"), Var("one"))), Var("one") -> IntV(1)), AppExp(Var("addx"), ILit(1))).right.get === IntV(2))
    assert(evalExp(Map(Var("addx") -> DProcV("x", BinOp(Plus, Var("x"), Var("one")))), AppExp(Var("addx"), ILit(1))).left.get.isInstanceOf[RuntimeException])
  }

  it should "eval declarations properly" in {
    assert(evalDecl(getEmptyEnv, Exp(BinOp(Plus, ILit(1), ILit(2)))).right.get === SingleEvalResult("-", getEmptyEnv, IntV(3)))
    assert(evalDecl(getEmptyEnv, MultiDecl(List(Decl("x", ILit(3))))).right.get === MultiEvalResult(List("x"), Map(Var("x") -> IntV(3)), List(IntV(3))))
    assert(evalDecl(getEmptyEnv, MultiDecl(List(Decl("x", ILit(3)), Decl("y", ILit(1))))).right.get
      === MultiEvalResult(List("x", "y"), Map(Var("x") -> IntV(3), Var("y") -> IntV(1)), List(IntV(3), IntV(1))))
    assert(evalDecl(Map(Var("x") -> IntV(10)), MultiDecl(List(Decl("x", ILit(3)), Decl("y", Var("x"))))).right.get
      === MultiEvalResult(List("x", "y"), Map(Var("x") -> IntV(3), Var("y") -> IntV(10)), List(IntV(3), IntV(10))))
    assert(evalDecl(getEmptyEnv, MultiDecl(List(Decl("addx", FunExp("x", BinOp(Plus, Var("x"), ILit(1))))))).right.get
      === MultiEvalResult(List("addx"), Map(Var("addx") -> ProcV("x", Map(), BinOp(Plus, Var("x"), ILit(1)))), List(ProcV("x", Map(), BinOp(Plus, Var("x"), ILit(1))))))
    evalDecl(getEmptyEnv, RecDecl("fact", "x", IfExp(BinOp(Lt, Var("x"), ILit(1)), ILit(1), BinOp(Mult, Var("x"), AppExp(Var("fact"), BinOp(Minus, Var("x"), ILit(1))))))).right.get ===
      SingleEvalResult("fact",Map(Var("fact") -> DProcV("x",IfExp(BinOp(Lt,Var("x"),ILit(1)),ILit(1),AppExp(Var("fact"),BinOp(Minus,Var("x"),ILit(1)))))),DProcV("x",IfExp(BinOp(Lt,Var("x"),ILit(1)),ILit(1),AppExp(Var("fact"),BinOp(Minus,Var("x"),ILit(1))))))
  }

  it should "throw Exception for invalid declaration" in {
    assert(evalDecl(getEmptyEnv, Exp(BinOp(And, ILit(1), BLit(true)))).left.get.isInstanceOf[RuntimeException])
    assert(evalDecl(getEmptyEnv, Exp(BinOp(And, ILit(1), ILit(2)))).left.get.isInstanceOf[RuntimeException])
    assert(evalDecl(getEmptyEnv, MultiDecl(List(Decl("x", BinOp(And, ILit(1), ILit(2)))))).left.get.isInstanceOf[RuntimeException])
    assert(evalExp(getEmptyEnv, LetExp("x", ILit(1), BinOp(Plus, Var("x"), Var("y")))).left.get.isInstanceOf[RuntimeException])
    assert(evalExp(getEmptyEnv, AppExp(Var("addx"), ILit(1))).left.get.isInstanceOf[RuntimeException])
  }
  */
}
