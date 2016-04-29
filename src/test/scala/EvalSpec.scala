import mlscala.Ast._
import mlscala.Eval._
import org.scalatest.{Matchers, FlatSpec}

class EvalSpec extends FlatSpec with Matchers {

  behavior of "Eval.scala"

  it should "ILit(_)" in {
    assert(applyPrim(Plus, ILit(1), ILit(3)) == Right(ILit(4)))
    assert(applyPrim(Mult, ILit(3), ILit(3)) == Right(ILit(9)))
  }

  it should "BLit(_)" in {
    assert(applyPrim(Lt, ILit(1), ILit(5)) == Right(BLit(true)))
    assert(applyPrim(Lt, ILit(10), ILit(5)) == Right(BLit(false)))
  }
}
