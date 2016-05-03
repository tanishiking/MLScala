import mlscala.Ast._
import mlscala.Eval._
import mlscala.Environment._
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
    assert(evalDecl(getEmptyEnv, Decl("x", BinOp(And, ILit(1), ILit(2)))).left.get.isInstanceOf[RuntimeException])
    assert(evalExp(getEmptyEnv, LetExp("x", ILit(1), BinOp(Plus, Var("x"), Var("y")))).left.get.isInstanceOf[RuntimeException])
  }

  it should "return tuple" in {
    assert(evalDecl(getEmptyEnv, Exp(BinOp(Plus, ILit(1), ILit(2)))).right.get == ("-", getEmptyEnv, ILit(3)))
  }

  it should "update env" in {
    assert(evalDecl(getEmptyEnv, Decl("x", ILit(3))).right.get == ("x", Map(Var("x") -> ILit(3)), ILit(3)))
  }
}
