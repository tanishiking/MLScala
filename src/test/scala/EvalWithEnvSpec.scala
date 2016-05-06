import mlscala.Ast._
import mlscala.Eval._
import mlscala.Environment._
import mlscala.EvalResult._
import org.scalatest.{Matchers, FlatSpec}

class EvalWithEnvSpec extends FlatSpec with Matchers {

  behavior of "Eval.scala and Environment.Scala"

  it should "IntV(5)" in {
    assert(evalExp(extendEnv(Var("x"), IntV(3), getEmptyEnv), BinOp(Plus, Var("x"), Var("x"))) == Right(IntV(6)))
  }

  it should "IntV(1)" in {
    assert(evalExp(getEmptyEnv, LetExp("x", ILit(1), Var("x"))) == Right(IntV(1)))
  }

  it should "throw Exception" in {
    assert(evalDecl(getEmptyEnv, Exp(BinOp(And, ILit(1), BLit(true)))).left.get.isInstanceOf[RuntimeException])
    assert(evalDecl(getEmptyEnv, Exp(BinOp(And, ILit(1), ILit(2)))).left.get.isInstanceOf[RuntimeException])
    assert(evalDecl(getEmptyEnv, MultiDecl(List(Decl("x", BinOp(And, ILit(1), ILit(2)))))).left.get.isInstanceOf[RuntimeException])
    assert(evalExp(getEmptyEnv, LetExp("x", ILit(1), BinOp(Plus, Var("x"), Var("y")))).left.get.isInstanceOf[RuntimeException])
  }

  it should "return tuple" in {
    assert(evalDecl(getEmptyEnv, Exp(BinOp(Plus, ILit(1), ILit(2)))).right.get == SingleEvalResult("-", getEmptyEnv, IntV(3)))
  }

  it should "update env" in {
    assert(evalDecl(getEmptyEnv, MultiDecl(List(Decl("x", ILit(3))))).right.get == MultiEvalResult(List("x"), Map(Var("x") -> IntV(3)), List(IntV(3))))
    assert(evalDecl(getEmptyEnv, MultiDecl(List(Decl("x", ILit(3)), Decl("y", ILit(1))))).right.get
      == MultiEvalResult(List("x", "y"), Map(Var("x") -> IntV(3), Var("y") -> IntV(1)), List(IntV(3), IntV(1))))
  }

  it should "y would equal to global env's x value" in {
    assert(evalDecl(Map(Var("x") -> IntV(10)), MultiDecl(List(Decl("x", ILit(3)), Decl("y", Var("x"))))).right.get
      == MultiEvalResult(List("x", "y"), Map(Var("x") -> IntV(3), Var("y") -> IntV(10)), List(IntV(3), IntV(10))))
  }

  it should "MultiDecl of functon" in {
    assert(evalDecl(getEmptyEnv, MultiDecl(List(Decl("addx", FunExp("x", BinOp(Plus, Var("x"), ILit(1))))))).right.get
      == MultiEvalResult(List("addx"), Map(Var("addx") -> ProcV("x", Map(), BinOp(Plus, Var("x"), ILit(1)))), List(ProcV("x", Map(), BinOp(Plus, Var("x"), ILit(1))))))
  }
}
