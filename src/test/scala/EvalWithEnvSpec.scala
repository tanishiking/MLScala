import mlscala.Ast._
import mlscala.Eval._
import mlscala.Environment._
import org.scalatest.{Matchers, FlatSpec}

class EvalWithEnvSpec extends FlatSpec with Matchers {

  behavior of "Eval.scala and Environment.Scala"

  it should "ILit(5)" in {
    assert(evalExp(extendEnv(Var("x"), ILit(3), getEmptyEnv), BinOp(Plus, Var("x"), Var("x"))) == Right(ILit(6)))
  }
}
