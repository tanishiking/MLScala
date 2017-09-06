package mlscala

import Ast._
import EvalResult._
import exceptions.{TypeMismatchException, VariableNotBoundException}
import org.scalatest.{FunSpec, Matchers}

class EvalSpec extends FunSpec with Matchers {
  describe("Eval") {
    describe(".applyPrim") {
      it ("should return Either.Right for valid input") {
        Eval.evalStmt(Environment.empty, TopExpr(BinOp(Plus, ILit(1), ILit(3)))) shouldBe Right(SingleEvalResult("-", Environment.empty, IntV(4)))
        Eval.evalStmt(Environment.empty, TopExpr(BinOp(Minus, ILit(3), ILit(5)))) shouldBe Right(SingleEvalResult("-", Environment.empty, IntV(-2)))
        Eval.evalStmt(Environment.empty, TopExpr(BinOp(Mult, ILit(3), ILit(3)))) shouldBe Right(SingleEvalResult("-", Environment.empty, IntV(9)))
        Eval.evalStmt(Environment.empty, TopExpr(BinOp(And, BLit(true), BLit(true)))) shouldBe Right(SingleEvalResult("-", Environment.empty, BoolV(true)))
        Eval.evalStmt(Environment.empty, TopExpr(BinOp(And, BLit(false), BLit(true)))) shouldBe Right(SingleEvalResult("-", Environment.empty, BoolV(false)))
        Eval.evalStmt(Environment.empty, TopExpr(BinOp(And, BLit(true), BLit(false)))) shouldBe Right(SingleEvalResult("-", Environment.empty, BoolV(false)))
        Eval.evalStmt(Environment.empty, TopExpr(BinOp(And, BLit(false), BLit(false)))) shouldBe Right(SingleEvalResult("-", Environment.empty, BoolV(false)))
        Eval.evalStmt(Environment.empty, TopExpr(BinOp(Or, BLit(true), BLit(true)))) shouldBe Right(SingleEvalResult("-", Environment.empty, BoolV(true)))
        Eval.evalStmt(Environment.empty, TopExpr(BinOp(Or, BLit(false), BLit(true)))) shouldBe Right(SingleEvalResult("-", Environment.empty, BoolV(true)))
        Eval.evalStmt(Environment.empty, TopExpr(BinOp(Or, BLit(true), BLit(false)))) shouldBe Right(SingleEvalResult("-", Environment.empty, BoolV(true)))
        Eval.evalStmt(Environment.empty, TopExpr(BinOp(Or, BLit(false), BLit(false)))) shouldBe Right(SingleEvalResult("-", Environment.empty, BoolV(false)))
        Eval.evalStmt(Environment.empty, TopExpr(BinOp(Lt, ILit(1), ILit(5)))) shouldBe Right(SingleEvalResult("-", Environment.empty, BoolV(true)))
        Eval.evalStmt(Environment.empty, TopExpr(BinOp(Lt, ILit(5), ILit(1)))) shouldBe Right(SingleEvalResult("-", Environment.empty, BoolV(false)))
        Eval.evalStmt(Environment.empty, TopExpr(BinOp(Lt, ILit(5), ILit(5)))) shouldBe Right(SingleEvalResult("-", Environment.empty, BoolV(false)))
      }

      it ("should return Either.Left for invalid input") {
        Eval.evalStmt(Environment.empty, TopExpr(BinOp(Plus, ILit(1), BLit(true)))).left.get.isInstanceOf[TypeMismatchException] shouldBe true
      }
    }

    describe(".evalExp") {
      describe("Var") {
        it ("should eval Var (from environment)") {
          Eval.evalStmt(Map(Var("x") -> IntV(3)), TopExpr(BinOp(Plus, Var("x"), Var("x")))) shouldBe Right(SingleEvalResult("-", Map(Var("x") -> IntV(3)), IntV(6)))
        }

        it ("should return left if not bounded variable refered") {
          Eval.evalStmt(Map(Var("x") -> IntV(3)), TopExpr(Var("y"))).left.get.isInstanceOf[VariableNotBoundException] shouldBe true
        }
      }

      describe("ILit") {
        it ("should eval int") {
          Eval.evalStmt(Environment.empty, TopExpr(ILit(1))) shouldBe Right(SingleEvalResult("-", Environment.empty, IntV(1)))
        }
      }

      describe("BLit") {
        it ("should eval boolean") {
          Eval.evalStmt(Environment.empty, TopExpr(BLit(true))) shouldBe Right(SingleEvalResult("-", Environment.empty, BoolV(true)))
        }
      }

      describe("ifExpr") {
        it ("should eval then/else expression depend on condition expr") {
          Eval.evalStmt(Environment.empty, TopExpr(IfExp(BLit(true), ILit(1), ILit(0)))) shouldBe Right(SingleEvalResult("-", Environment.empty, IntV(1)))
          Eval.evalStmt(Environment.empty, TopExpr(IfExp(BLit(false), ILit(1), ILit(0)))) shouldBe Right(SingleEvalResult("-", Environment.empty, IntV(0)))
          val env = Map(Var("x") -> BoolV(true))
          Eval.evalStmt(env, TopExpr(IfExp(Var("x"), ILit(1), ILit(0)))) shouldBe Right(SingleEvalResult("-", env, IntV(1)))
        }

        it ("should return left contains runtime exception when invalid value found in cond expr") {
          Eval.evalStmt(Environment.empty, TopExpr(IfExp(ILit(1), ILit(1), ILit(0)))).left.get.isInstanceOf[RuntimeException] shouldBe true
        }
      }

      describe("letExpr") {
        it ("should eval let expression") {
          Eval.evalStmt(Environment.empty, TopExpr(LetExp("x", ILit(1), Var("x")))) shouldBe Right(SingleEvalResult("-", Environment.empty, IntV(1)))
        }
      }

      describe("funExpr") {
        it ("should eval funExpr") {
          Eval.evalStmt(Environment.empty, TopExpr(FunExp("x", Var("x")))) shouldBe Right(SingleEvalResult("-", Environment.empty, ProcV("x", Map(), Var("x"))))
          Eval.evalStmt(Map(Var("y") -> IntV(1)), TopExpr(FunExp("x", Var("x")))) shouldBe Right(SingleEvalResult("-", Map(Var("y") -> IntV(1)), ProcV("x", Map(Var("y") -> IntV(1)), Var("x"))))
        }
      }

      describe("dfunExpr") {
        it ("should eval dfunExpr") {
          Eval.evalStmt(Environment.empty, TopExpr(DFunExp("x", Var("x")))) shouldBe Right(SingleEvalResult("-", Environment.empty, DProcV("x", Var("x"))))
        }
      }

      describe("AppExpr") {
        it ("should eval app expr") {
          {
            val env = Map(Var("addx") -> ProcV("x", Map(), BinOp(Plus, Var("x"), ILit(1))))
            Eval.evalStmt(env, TopExpr(AppExp(Var("addx"), ILit(1)))) shouldBe
              Right(SingleEvalResult("-", env, IntV(2)))
          }
          {
            val env = Map(Var("addx") -> ProcV("x", Map(Var("y") -> IntV(100)), BinOp(Plus, Var("x"), Var("y"))))
            Eval.evalStmt(env, TopExpr(LetExp("y", ILit(0), AppExp(Var("addx"), ILit(1))))) shouldBe
              Right(SingleEvalResult("-", env, IntV(101)))
          }
        }

        it ("should eval dfun (read from current env)") {
          {
            val env = Map(
              Var("one") -> IntV(1),
              Var("addx") -> DProcV("x", BinOp(Plus, Var("x"), Var("one")))
            )
            Eval.evalStmt(env, TopExpr(AppExp(Var("addx"), ILit(1)))) shouldBe
              Right(SingleEvalResult("-", env, IntV(2)))

            Eval.evalStmt(
              Map(Var("addx") -> DProcV("x", BinOp(Plus, Var("x"), Var("one")))),
              TopExpr(AppExp(Var("addx"), ILit(1)))
            ).left.get.isInstanceOf[RuntimeException]
          }
        }
      }
    }
  }

  /*
  it should "eval declarations properly" in {
    assert(evalDecl(empty, Exp(BinOp(Plus, ILit(1), ILit(2)))).right.get === SingleEvalResult("-", empty, ILit(3)))
    assert(evalDecl(empty, MultiDecl(List(Decl("x", ILit(3))))).right.get === MultiEvalResult(List("x"), Map(Var("x") -> ILit(3)), List(ILit(3))))
    assert(evalDecl(empty, MultiDecl(List(Decl("x", ILit(3)), Decl("y", ILit(1))))).right.get
      === MultiEvalResult(List("x", "y"), Map(Var("x") -> ILit(3), Var("y") -> ILit(1)), List(ILit(3), ILit(1))))
    assert(evalDecl(Map(Var("x") -> ILit(10)), MultiDecl(List(Decl("x", ILit(3)), Decl("y", Var("x"))))).right.get
      === MultiEvalResult(List("x", "y"), Map(Var("x") -> ILit(3), Var("y") -> ILit(10)), List(ILit(3), ILit(10))))
    assert(evalDecl(empty, MultiDecl(List(Decl("addx", FunExp("x", BinOp(Plus, Var("x"), ILit(1))))))).right.get
      === MultiEvalResult(List("addx"), Map(Var("addx") -> ProcV("x", Map(), BinOp(Plus, Var("x"), ILit(1)))), List(ProcV("x", Map(), BinOp(Plus, Var("x"), ILit(1))))))
    evalDecl(empty, RecDecl("fact", "x", IfExp(BinOp(Lt, Var("x"), ILit(1)), ILit(1), BinOp(Mult, Var("x"), AppExp(Var("fact"), BinOp(Minus, Var("x"), ILit(1))))))).right.get ===
      SingleEvalResult("fact",Map(Var("fact") -> DProcV("x",IfExp(BinOp(Lt,Var("x"),ILit(1)),ILit(1),AppExp(Var("fact"),BinOp(Minus,Var("x"),ILit(1)))))),DProcV("x",IfExp(BinOp(Lt,Var("x"),ILit(1)),ILit(1),AppExp(Var("fact"),BinOp(Minus,Var("x"),ILit(1))))))
  }

  it should "throw Exception for invalid declaration" in {
    assert(evalDecl(empty, MultiDecl(List(Decl("x", BinOp(And, ILit(1), ILit(2)))))).left.get.isInstanceOf[RuntimeException])
    assert(evalStmt(empty, LetExp("x", ILit(1), BinOp(Plus, Var("x"), Var("y")))).left.get.isInstanceOf[RuntimeException])
    assert(evalStmt(empty, AppExp(Var("addx"), ILit(1))).left.get.isInstanceOf[RuntimeException])
  }
  */
}
