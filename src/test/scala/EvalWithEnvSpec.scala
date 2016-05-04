import mlscala.Ast._
import mlscala.Eval._
import mlscala.Environment._
import mlscala.EvalResult.{SingleEvalResult, MultiEvalResult}
import org.scalatest.{Matchers, FlatSpec}

class EvalWithEnvSpec extends FlatSpec with Matchers {

  behavior of "Eval.scala and Environment.Scala"

  it should "ILit(5)" in {
    assert(evalExp(extendEnv(Var("x"), ILit(3), getEmptyEnv), BinOp(Plus, Var("x"), Var("x"))) == Right(ILit(6)))
  }

  it should "ILit(1)" in {
    assert(evalExp(getEmptyEnv, LetExp("x", ILit(1), Var("x"))) == Right(ILit(1)))
  }

  it should "throw Exception" in {
    assert(evalDecl(getEmptyEnv, Exp(BinOp(And, ILit(1), BLit(true)))).left.get.isInstanceOf[RuntimeException])
    assert(evalDecl(getEmptyEnv, Exp(BinOp(And, ILit(1), ILit(2)))).left.get.isInstanceOf[RuntimeException])
    assert(evalDecl(getEmptyEnv, MultiDecl(List(Decl("x", BinOp(And, ILit(1), ILit(2)))))).left.get.isInstanceOf[RuntimeException])
    assert(evalExp(getEmptyEnv, LetExp("x", ILit(1), BinOp(Plus, Var("x"), Var("y")))).left.get.isInstanceOf[RuntimeException])
  }

  it should "return tuple" in {
    assert(evalDecl(getEmptyEnv, Exp(BinOp(Plus, ILit(1), ILit(2)))).right.get == SingleEvalResult("-", getEmptyEnv, ILit(3)))
  }

  it should "update env" in {
    assert(evalDecl(getEmptyEnv, MultiDecl(List(Decl("x", ILit(3))))).right.get == MultiEvalResult(List("x"), Map(Var("x") -> ILit(3)), List(ILit(3))))
    assert(evalDecl(getEmptyEnv, MultiDecl(List(Decl("x", ILit(3)), Decl("y", ILit(1))))).right.get
      == MultiEvalResult(List("x", "y"), Map(Var("x") -> ILit(3), Var("y") -> ILit(1)), List(ILit(3), ILit(1))))
  }

  it should "y would equal to global env's x value" in {
    assert(evalDecl(Map(Var("x") -> ILit(10)), MultiDecl(List(Decl("x", ILit(3)), Decl("y", Var("x"))))).right.get
      == MultiEvalResult(List("x", "y"), Map(Var("x") -> ILit(3), Var("y") -> ILit(10)), List(ILit(3), ILit(10))))
  }
}
